open Sast
module A = Ast

let check =
  let rec check_stmt_block (symbol_table : 'a) : A.expr -> 'a * stmt list =
    function
    | A.Nil -> (symbol_table, [])
    | A.Cons (hd, tl) ->
        let symbol_table, stmt = check_stmt symbol_table hd in
        let symbol_table, new_tail = check_stmt_block symbol_table tl in
        (symbol_table, stmt :: new_tail)
    | _ -> raise (Failure "Invalid statement block")
  and check_expr_list symbol_table : A.expr -> expr list = function
    | Nil -> []
    | Cons (hd, tl) ->
        check_expr symbol_table hd :: check_expr_list symbol_table tl
    | _ -> raise (Failure "Invalid expression list")
  and check_expr symbol_table : A.expr -> expr =
    let rec map_quotted = function
      | A.CharLit c -> CharLit c
      | A.StrLit c -> StrLit c
      | A.IntLit c -> IntLit c
      | A.Nil -> Nil
      | A.Id name -> Id name
      | A.Cons (hd, tl) -> Cons (map_quotted hd, map_quotted tl)
      | A.Quote expr -> Quote (map_quotted expr)
      | A.Expansion -> raise (Failure "Invalid expansion dots")
    in

    function
    | A.Id name ->
        if Symtable.mem name symbol_table then Id name
        else raise (Failure ("Undefined variable " ^ name))
    | A.Quote expr -> Quote (map_quotted expr)
    | A.Expansion -> raise (Failure "Invalid expansion dots")
    | A.Cons (expr, args) -> (
        match expr with
        | A.Id id when Symtable.mem id symbol_table ->
            FunCall (Id id, check_expr_list symbol_table args)
        | A.Id "if" -> (
            match args with
            | Cons (predicate, Cons (then_clause, maybeElse)) ->
                If
                  ( check_expr symbol_table predicate,
                    check_expr symbol_table then_clause,
                    match maybeElse with
                    | Cons (else_clause, Nil) ->
                        Some (check_expr symbol_table else_clause)
                    | Nil -> None
                    | _ -> raise (Failure "Invalid else caluse") )
            | _ -> raise (Failure "Invalid if expression") )
        | A.Id "let" -> (
            let rec check_let_bindings = function
              | A.Nil -> []
              | A.Cons (Cons (Id name, Cons (value, Nil)), rest) ->
                  (name, check_expr symbol_table value)
                  :: check_let_bindings rest
              | _ -> raise (Failure "Invalid let binding list")
            in

            match args with
            | A.Cons (bindings, body) ->
                let bindings = check_let_bindings bindings in
                let new_symbol_table =
                  List.fold_left
                    (fun symbol_table binding ->
                      let name, value = binding in
                      Symtable.add name () symbol_table)
                    (Symtable.push symbol_table)
                    bindings
                in
                let _, stmts = check_stmt_block new_symbol_table body in
                Let (bindings, stmts)
            | _ -> raise (Failure "Invalid let expression") )
        | A.Id "begin" ->
            let _, stmts = check_stmt_block (Symtable.push symbol_table) args in
            Begin stmts
        | A.Id "lambda" -> (
            let rec check_lambda_bindings = function
              | A.Nil -> []
              | A.Cons (A.Id name, rest) -> name :: check_lambda_bindings rest
              | _ -> raise (Failure "Invalid lambda argument list")
            in

            match args with
            | Cons (arg_list, body) ->
                let bindings = check_lambda_bindings arg_list in
                let new_symbol_table =
                  List.fold_left
                    (fun symbol_table name -> Symtable.add name () symbol_table)
                    (Symtable.push symbol_table)
                    bindings
                in

                let _, stmts = check_stmt_block new_symbol_table body in
                Lambda (bindings, stmts)
            | _ -> raise (Failure "Invalid lambda") )
        | A.Id name -> raise (Failure ("Undefined function: " ^ name))
        | Cons (_, _) as expr ->
            FunCall
              (check_expr symbol_table expr, check_expr_list symbol_table args)
        | _ as v -> raise (Failure ("Invalid function: " ^ A.string_of_ast v)) )
    (* literals are self quotted *)
    | expr -> map_quotted expr
  and check_stmt (symbol_table : 'a) : A.expr -> 'a * stmt = function
    | A.Cons (A.Id "define", Cons (Id name, Cons (value, Nil))) ->
        let new_symbol_table = Symtable.add name () symbol_table in
        (new_symbol_table, Define (name, check_expr new_symbol_table value))
    | A.Cons (A.Id "define", _) -> raise (Failure "Invalid define statement")
    | A.Cons (A.Id "set!", Cons (Id name, Cons (value, Nil))) ->
        if Symtable.mem name symbol_table then
          (symbol_table, Set (name, check_expr symbol_table value))
        else raise (Failure ("Variable " ^ name ^ " is undefined"))
    | A.Cons (A.Id "set!", _) -> raise (Failure "Invalid set! statement")
    | expr -> (symbol_table, Expr (check_expr symbol_table expr))
  in

  let rec check_iter symbol_table : A.expr list -> stmt list = function
    | [] -> []
    | hd :: tl ->
        let symbol_table, new_head = check_stmt symbol_table hd in
        new_head :: check_iter symbol_table tl
  in

  let builtin_variables = Symtable.from [ ("=", ()); ("display", ()) ] in
  check_iter builtin_variables
