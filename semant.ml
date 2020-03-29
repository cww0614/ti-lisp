open Sast
module A = Ast

type value_type = Function of value_type * int * int | Value | Void | Any

let check =
  let rec check_stmt_block (symbol_table : 'a) :
      A.expr -> 'a * value_type * stmt list = function
    | A.Nil -> (symbol_table, Void, [])
    | A.Cons (hd, Nil) ->
        let symbol_table, stmt_type, stmt = check_stmt symbol_table hd in
        (symbol_table, stmt_type, [ stmt ])
    | A.Cons (hd, tl) ->
        let symbol_table, _, stmt = check_stmt symbol_table hd in
        let symbol_table, value_type, new_tail =
          check_stmt_block symbol_table tl
        in
        (symbol_table, value_type, stmt :: new_tail)
    | _ -> raise (Failure "Invalid statement block")
  and check_expr_list symbol_table : A.expr -> expr list = function
    | Nil -> []
    | Cons (hd, tl) ->
        let _, expr = check_expr symbol_table hd in
        expr :: check_expr_list symbol_table tl
    | _ -> raise (Failure "Invalid expression list")
  and check_expr symbol_table : A.expr -> value_type * expr =
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
        if Symtable.mem name symbol_table then (Value, Id name)
        else raise (Failure ("Undefined variable " ^ name))
    | A.Quote expr -> (Value, Quote (map_quotted expr))
    | A.Expansion -> raise (Failure "Invalid expansion dots")
    | A.Cons (expr, args) -> (
        match expr with
        | A.Id id when Symtable.mem id symbol_table -> (
            match Option.get (Symtable.find id symbol_table) with
            | Function (ret, min, max) ->
                let args = check_expr_list symbol_table args in
                let arg_len = List.length args in
                if min <= arg_len && arg_len <= max then
                  (ret, FunCall (Id id, args))
                else
                  raise
                    (Failure
                       ( "Function " ^ id ^ " takes " ^ string_of_int min
                       ^ " to " ^ string_of_int max ^ " arguments, instead of "
                       ^ string_of_int arg_len ))
            | Any -> (Any, FunCall (Id id, check_expr_list symbol_table args))
            | _ -> raise (Failure (id ^ " is not a function")) )
        | A.Id "if" -> (
            match args with
            | Cons (predicate, Cons (then_clause, maybeElse)) -> (
                let predicate_type, predicate =
                  check_expr symbol_table predicate
                in
                let then_type, then_clause =
                  check_expr symbol_table then_clause
                in

                match maybeElse with
                | Cons (else_clause, Nil) ->
                    let else_type, else_clause =
                      check_expr symbol_table else_clause
                    in
                    ( ( match (then_type, else_type) with
                      | Function (tp1, min1, max1), Function (tp2, min2, max2)
                        when tp1 = tp2 && min1 = min2 && max1 = max2 ->
                          Function (tp1, min1, max1)
                      | Value, Value -> Value
                      | _ -> Any ),
                      If (predicate, then_clause, Some else_clause) )
                | Nil -> (then_type, If (predicate, then_clause, None))
                | _ -> raise (Failure "Invalid else caluse") )
            | _ -> raise (Failure "Invalid if expression") )
        | A.Id "let" -> (
            let rec check_let_bindings = function
              | A.Nil -> []
              | A.Cons (Cons (Id name, Cons (value, Nil)), rest) ->
                  let value_type, expr = check_expr symbol_table value in
                  (name, value_type, expr) :: check_let_bindings rest
              | _ -> raise (Failure "Invalid let binding list")
            in

            match args with
            | A.Cons (bindings, body) ->
                let bindings = check_let_bindings bindings in
                let new_symbol_table =
                  List.fold_left
                    (fun symbol_table binding ->
                      let name, value_type, _ = binding in
                      Symtable.add name value_type symbol_table)
                    (Symtable.push symbol_table)
                    bindings
                in
                let _, stmt_type, stmts =
                  check_stmt_block new_symbol_table body
                in
                ( stmt_type,
                  Let
                    ( List.map
                        (function name, tp, expr -> (name, expr))
                        bindings,
                      stmts ) )
            | _ -> raise (Failure "Invalid let expression") )
        | A.Id "begin" ->
            let _, stmt_type, stmts =
              check_stmt_block (Symtable.push symbol_table) args
            in
            (stmt_type, Begin stmts)
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
                    (fun symbol_table name ->
                      Symtable.add name Any symbol_table)
                    (Symtable.push symbol_table)
                    bindings
                in

                let _, stmt_type, stmts =
                  check_stmt_block new_symbol_table body
                in
                ( Function
                    (stmt_type, List.length bindings, List.length bindings),
                  Lambda (bindings, stmts) )
            | _ -> raise (Failure "Invalid lambda") )
        | A.Id name -> raise (Failure ("Undefined function: " ^ name))
        | Cons (_, _) as expr -> (
            let func_type, func = check_expr symbol_table expr in
            match func_type with
            | Function (ret, min, max) ->
                let args = check_expr_list symbol_table args in
                let arg_len = List.length args in
                if min <= arg_len && max >= arg_len then
                  (ret, FunCall (func, args))
                else
                  raise
                    (Failure
                       ( "Unnamed function takes " ^ string_of_int min ^ " to "
                       ^ string_of_int max ^ " arguments, instead of "
                       ^ string_of_int arg_len ))
            | Any -> (Any, FunCall (func, check_expr_list symbol_table args))
            | _ -> raise (Failure "Calling to a non-function") )
        | _ as v -> raise (Failure ("Invalid function: " ^ A.string_of_ast v)) )
    (* literals are self quotted *)
    | expr -> (Value, map_quotted expr)
  and check_stmt (symbol_table : 'a) : A.expr -> 'a * value_type * stmt =
    function
    | A.Cons (A.Id "define", Cons (Id name, Cons (value, Nil))) ->
        let temp_symbol_table = Symtable.add name Any symbol_table in
        let value_type, _ = check_expr temp_symbol_table value in
        let new_symbol_table = Symtable.add name value_type symbol_table in
        let value_type, value = check_expr new_symbol_table value in
        (new_symbol_table, Void, Define (name, value))
    | A.Cons (A.Id "define", _) -> raise (Failure "Invalid define statement")
    | A.Cons (A.Id "set!", Cons (Id name, Cons (value, Nil))) ->
        if Symtable.mem name symbol_table then
          let value_type, value = check_expr symbol_table value in
          let new_symbol_table = Symtable.add name value_type symbol_table in
          (new_symbol_table, Void, Set (name, value))
        else raise (Failure ("Variable " ^ name ^ " is undefined"))
    | A.Cons (A.Id "set!", _) -> raise (Failure "Invalid set! statement")
    | expr ->
        let expr_type, expr = check_expr symbol_table expr in
        (symbol_table, expr_type, Expr expr)
  in

  let rec check_iter symbol_table : A.expr list -> stmt list = function
    | [] -> []
    | hd :: tl ->
        let symbol_table, _, new_head = check_stmt symbol_table hd in
        new_head :: check_iter symbol_table tl
  in

  let builtin_variables =
    Symtable.from
      [
        ("+", Function (Value, 2, 2));
        ("-", Function (Value, 2, 2));
        ("*", Function (Value, 2, 2));
        ("/", Function (Value, 2, 2));
        ("=", Function (Value, 2, 2));
        ("display", Function (Void, 1, 256));
        ("true", Value);
        ("false", Value);
      ]
  in
  check_iter builtin_variables
