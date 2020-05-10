open Ast
module StringMap = Map.Make (String)

type macro_rule = { pattern : expr; body : expr }

type macro_def = { literals : string list; rules : macro_rule list }

type symbol_table = macro_def Symtable.symbol_table

let analyze_syntax_rules : expr -> macro_def =
  let analyze_rule : expr -> macro_rule = function
    | Cons (pattern, Cons (body, Nil)) -> { pattern; body }
    | _ -> raise (Failure "Invalid syntax-rules pattern")
  in

  function
  | Cons (Id "syntax-rules", Cons (literals, rules)) ->
      {
        literals =
          List.map
            (function
              | Id x -> x | _ -> raise (Failure "Invalid literal list in macro"))
            (cons_to_list literals);
        rules = List.map analyze_rule (cons_to_list rules);
      }
  | _ -> raise (Failure "Invalid syntax-rules")

let builtin_macros : symbol_table =
  Symtable.from
    (List.map
       (function
         | name, rule_text ->
             let lexbuf = Lexing.from_string rule_text in
             let expr = Parser.program Scanner.token lexbuf in
             (name, analyze_syntax_rules (List.hd expr)))
       (* Some of the macro definitions are adpated from r5rs specification *)
       [
         ( "cond",
           {| (syntax-rules (else)
                ((_ (predicate body ...) clauses ...)
                 (if predicate
                     (begin body ...)
                     (cond clauses ...)))
                ((_ (predicate body ...))
                 (if predicate
                     (begin body ...)))
                ((_ (else body ...))
                 (begin body ...))) |}
         );
         ( "define",
           {| (syntax-rules ()
                ((_ (name args ...) body ...)
                 (define name
                   (lambda (args ...)
                     body ...)))
                ((_ (name) body ...)
                 (define name
                   (lambda ()
                     body ...)))) |}
         );
         ( "and",
           {| (syntax-rules ()
                ((_) true)
                ((_ test) test)
                ((_ test1 test2 ...)
                 (if test1 (and test2 ...) false))) |}
         );
         ( "or",
           {| (syntax-rules ()
                ((_) false)
                ((_ test) test)
                ((_ test1 test2 ...)
                 (let ((x test1))
                   (if x x (or test2 ...))))) |}
         );
         ( "let*",
           {| (syntax-rules ()
                ((_ ((name value)) body ...)
                 (let ((name value)) body ...))
                ((_ ((name value) more ...) body ...)
                 (let ((name value))
                   (let* (more ...)
                     body ...)))) |}
         );
         ( "let",
           {| (syntax-rules ()
                ((_ tag ((name val) ...) body ...)
                 ((let ((tag (lambda (name ...) body ...))) tag) val ...)
         )) |}
         );
         ( "+",
           {| (syntax-rules ()
                ((+ a b c ...) (+ a (+ b c ...)))) |}
         );
         ( "*",
           {| (syntax-rules ()
                ((* a b c ...) (* a (* b c ...)))) |}
         );
         ( "display",
         {| (syntax-rules ()
                ((display a b ...) (begin (display a) (display b ...)))) |}
         );
       ])

let expand (symbol_table : symbol_table) (expr : expr) : symbol_table * expr =
  (* This is the expanding "atom" function. For an input expression:

     If it is a macro definition, return the updated symbol table and
     None. (to indicate that the macro definition is deleted from
     source code)

     If it is a macro form, then apply the macro, return the unchanged
     symbol table and expanded expression.

     Otherwise, it return the symbol table and the input unchanged *)
  let rec expand1 (symbol_table : symbol_table) :
      expr -> symbol_table * expr option =
    (* check if the expression expr matches the pattern (given a
       list of literals). If it matches, returns a map that stores
       the matched expression for each identifier in the
       pattern. Otherwise, it returns None. *)
    let match_rule (literals : string list) (pattern : expr) (expr : expr) :
        expr StringMap.t option =
      let rec match_rule_iter (map : expr StringMap.t) (literals : string list)
          (pattern : expr) (expr : expr) : expr StringMap.t option =
        match (pattern, expr) with
        | Id id1, Id id2 when List.mem id2 literals ->
            if id1 = id2 then Some map else None
        | Id "_", _ -> Some map
        | Id id, v when not (List.mem id literals) ->
            Some (StringMap.add id v map)
        | Cons (Id id, Cons (Expansion, Nil)), (Cons (_, _) as rest_form) ->
            Some (StringMap.add id rest_form map)
        | ( Cons ((Cons (_, _) as cons), Cons (Expansion, Nil)),
            (Cons (Cons (_, _), _) as rest_form) ) ->
            let rec match_destruct_binding (pattern : expr) (expr : expr) :
                expr list StringMap.t =
              match expr with
              | Cons (hd, tl) ->
                  let hd_m =
                    match
                      match_rule_iter StringMap.empty literals pattern hd
                    with
                    | Some map -> map
                    | None ->
                        raise (Failure "Invalid destructing match pattern")
                  in
                  let tl_m = match_destruct_binding pattern tl in
                  StringMap.merge
                    (fun key v1 v2 ->
                      match v1 with
                      | Some v ->
                          let v2 = match v2 with Some v -> v | None -> [] in
                          Some (v :: v2)
                      | None -> v2)
                    hd_m tl_m
              | Nil -> StringMap.empty
              | _ -> raise (Failure "Invalid destructing match pattern")
            in
            let binding_map = match_destruct_binding cons rest_form in
            let map =
              StringMap.fold
                (fun id value map -> StringMap.add id (list_to_cons value) map)
                binding_map map
            in
            Some map
        | Cons (hd1, tl1), Cons (hd2, tl2) -> (
            match match_rule_iter map literals hd1 hd2 with
            | Some map -> match_rule_iter map literals tl1 tl2
            | None -> None )
        | Nil, Nil -> Some map
        | _ -> None
      in

      match_rule_iter StringMap.empty literals pattern expr
    in

    (* Do expansion according to the identifier-expression map *)
    let rec replace_rule (map : expr StringMap.t) : expr -> expr = function
      | Id id as v -> if StringMap.mem id map then StringMap.find id map else v
      | Cons (Id id, Cons (Expansion, rest)) when StringMap.mem id map ->
          cons_concat (StringMap.find id map) (replace_rule map rest)
      | Cons (hd, tl) -> Cons (replace_rule map hd, replace_rule map tl)
      | v -> v
    in

    let rec find_first (f : 'a -> 'b option) : 'a list -> 'b option = function
      | [] -> None
      | hd :: tl -> (
          match f hd with Some v as s -> s | None -> find_first f tl )
    in

    function
    | Cons (Id "define-syntax", Cons (Id name, Cons (body, Nil))) ->
        (Symtable.add name (analyze_syntax_rules body) symbol_table, None)
    | Cons (Id id, expr) as cons -> (
        match Symtable.find id symbol_table with
        | Some { literals; rules } -> (
            match
              find_first
                (fun rule ->
                  match match_rule literals rule.pattern cons with
                  | Some map -> Some (rule, map)
                  | None -> None)
                rules
            with
            | Some (rule, map) ->
                (* recursively expansion *)
                expand1 symbol_table (replace_rule map rule.body)
            | None -> (symbol_table, Some cons) )
        | None -> (symbol_table, Some (Cons (Id id, expr))) )
    | v -> (symbol_table, Some v)
  in

  (* apply expand1 to every element recursively in a Cons-list, and
     update the symbol table in the process *)
  let rec expand_over_list (symbol_table : symbol_table) (expr : expr) :
      symbol_table * expr =
    let rec expand_over_list_iter symbol_table = function
      (* New level of list *)
      | Cons (Cons (head, tail), tail2) ->
          let symbol_table, new_head =
            expand_over_list (Symtable.push symbol_table) (Cons (head, tail))
          in
          let symbol_table, new_tail =
            expand_over_list_iter symbol_table tail2
          in
          (symbol_table, Cons (new_head, new_tail))
      (* Normal list element *)
      | Cons (head, tail) -> (
          let symbol_table, new_head = expand1 symbol_table head in
          let symbol_table, new_tail =
            expand_over_list_iter symbol_table tail
          in
          match new_head with
          | Some new_head -> (symbol_table, Cons (head, new_tail))
          | None -> (symbol_table, new_tail) )
      | v -> (symbol_table, v)
    in

    (* First check if the expression itself is a macro form *)
    let symbol_table, expanded = expand1 symbol_table expr in
    (* check if arguments are macro forms recursively *)
    match expanded with
    | Some expanded -> expand_over_list_iter symbol_table expanded
    | None -> (symbol_table, Nil)
  in

  expand_over_list symbol_table expr

(* Apply expand to a list of expression sequentially *)
let expand_all (exprs : expr list) : expr list =
  let _, exprs = Utils.fold_map expand builtin_macros exprs in
  exprs
