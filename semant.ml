open Sast
module A = Ast
module StringMap = Map.Make (String)

type value_type =
  (* return type, min number of args, max number of args *)
  | Function of value_type * int * int
  | Value
  | Any

type symbol_table = value_type Symtable.symbol_table

let check (expr_list: A.expr list): stmt list =
  let is_function (name : string) (symbol_table : symbol_table) : bool =
    match Symtable.find name symbol_table with
    | Some (Function (_, _, _)) -> true
    | Some Any -> true
    | _ -> false
  in

  let rec check_stmt_block (symbol_table : symbol_table) :
      A.expr -> symbol_table * value_type * stmt list = function
    | A.Cons (hd, A.Nil) ->
        let symbol_table, stmt_type, stmt = check_stmt symbol_table hd in
        (symbol_table, stmt_type, [ stmt ])
    | A.Cons (hd, tl) ->
        let symbol_table, _, stmt = check_stmt symbol_table hd in
        let symbol_table, value_type, new_tail =
          check_stmt_block symbol_table tl
        in
        (symbol_table, value_type, stmt :: new_tail)
    | _ -> raise (Failure "Invalid statement block")
  and check_expr_list (symbol_table : symbol_table) : A.expr -> expr list =
    function
    | A.Nil -> []
    | A.Cons (hd, tl) ->
        let _, expr = check_expr symbol_table hd in
        expr :: check_expr_list symbol_table tl
    | _ -> raise (Failure "Invalid expression list")
  and check_expr (symbol_table : symbol_table) : A.expr -> value_type * expr =
    (* determine the semantic meaning of quote (could be symbol,
       lists, cons) *)
    let rec quote_expr : A.expr -> expr = function
      (* literals are said to be self-quoted *)
      | A.CharLit c -> CharLit c
      | A.StrLit c -> StrLit c
      | A.IntLit c -> IntLit c
      | A.Nil -> Nil
      | A.Id name -> Symbol name
      | A.Cons (hd, tl) -> Cons (quote_expr hd, quote_expr tl)
      (* nested quote is converted to '(quote something) *)
      | A.Quote expr -> Cons (Id "quote", quote_expr expr)
      | A.Expansion -> raise (Failure "Invalid expansion dots")
    in

    function
    | A.Id name -> (
        match name with
        | _ when Symtable.mem name symbol_table ->
            ( ( match Symtable.find name symbol_table with
              | Some tp -> tp
              | None -> raise (Failure "Invariant violation") ),
              Id name )
        | "true" -> (Value, BoolLit true)
        | "false" -> (Value, BoolLit false)
        | _ -> raise (Failure ("Undefined variable " ^ name)) )
    | A.Quote expr -> (Value, quote_expr expr)
    | A.Expansion -> raise (Failure "Invalid expansion dots")
    (* Function call like expression *)
    | A.Cons (expr, args) -> (
        match expr with
        | A.Id id when is_function id symbol_table -> (
            match Symtable.find id symbol_table with
            | Some (Function (ret, min, max)) ->
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
            | Some Any ->
                (Any, FunCall (Id id, check_expr_list symbol_table args))
            | _ -> raise (Failure (id ^ " is not a function")) )
        | A.Id "if" -> (
            match args with
            | A.Cons (predicate, A.Cons (then_clause, maybeElse)) -> (
                let predicate_type, predicate =
                  check_expr symbol_table predicate
                in
                let then_type, then_clause =
                  check_expr symbol_table then_clause
                in

                match maybeElse with
                | A.Cons (else_clause, A.Nil) ->
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
                | A.Nil -> (then_type, If (predicate, then_clause, None))
                | _ -> raise (Failure "Invalid else caluse") )
            | _ -> raise (Failure "Invalid if expression") )
        | A.Id "let" -> (
            (* check if the bindings in the let are well formed *)
            let rec convert_let_bindings = function
              | A.Nil -> []
              | A.Cons (A.Cons (A.Id name, A.Cons (value, A.Nil)), rest) ->
                  (name, value) :: convert_let_bindings rest
              | _ -> raise (Failure "Invalid let binding list")
            in

            let rec check_let_bindings symbol_table =
              List.map (fun binding ->
                  let name, value = binding in
                  let value_type, expr = check_expr symbol_table value in
                  (name, value_type, expr))
            in

            (* check for duplicates in let bindings *)
            let rec check_let_duplicates bindings map =
              match bindings with
              | (name, _, _) :: rest ->
                  if
                    StringMap.exists
                      (fun key _ -> if key = name then true else false)
                      map
                  then raise (Failure "Duplications in let binding list")
                  else
                    let new_map = StringMap.add name name map in
                    check_let_duplicates rest new_map
              | _ -> None
            in

            match args with
            | A.Cons (bindings, body) ->
                let bindings = convert_let_bindings bindings in
                (* First fill Any for every variable *)
                let new_symbol_table =
                  List.fold_left
                    (fun symbol_table binding ->
                      let name, _ = binding in
                      Symtable.add name Any symbol_table)
                    (Symtable.push symbol_table)
                    bindings
                in

                let bindings = check_let_bindings new_symbol_table bindings in
                (* Then fill conrete types for variables *)
                let new_symbol_table =
                  List.fold_left
                    (fun symbol_table binding ->
                      let name, tp, _ = binding in
                      Symtable.add name tp symbol_table)
                    new_symbol_table bindings
                in

                (* check for let duplicates *)
                let map = StringMap.empty in
                let _ = check_let_duplicates bindings map in

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
            (* check if the arguments of a lambda are well formed *)
            let rec check_lambda_bindings = function
              | A.Nil -> []
              | A.Cons (A.Id name, rest) -> name :: check_lambda_bindings rest
              | _ -> raise (Failure "Invalid lambda argument list")
            in

            (* check for duplicates in lambda arguments *)
            let rec check_lambda_duplicates bindings map =
              match bindings with
              | name :: rest ->
                  if
                    StringMap.exists
                      (fun key _ -> if key = name then true else false)
                      map
                  then raise (Failure "Duplications in lambda argument list")
                  else
                    let new_map = StringMap.add name name map in
                    check_lambda_duplicates rest new_map
              | _ -> None
            in

            match args with
            | A.Cons (arg_list, body) ->
                let bindings = check_lambda_bindings arg_list in

                (* check for lambda duplicates *)
                let map = StringMap.empty in
                let _ = check_lambda_duplicates bindings map in

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
        | A.Cons (_, _) as expr -> (
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
    | expr -> (Value, quote_expr expr)
  and check_stmt (symbol_table : symbol_table) :
      A.expr -> symbol_table * value_type * stmt = function
    (* functions have higher precedence than set! and define *)
    | A.Cons (A.Id id, _) as expr when is_function id symbol_table ->
        let expr_type, expr = check_expr symbol_table expr in
        (symbol_table, expr_type, Expr expr)
    | A.Cons (A.Id "define", A.Cons (A.Id name, A.Cons (value, A.Nil))) ->
        (* Helper function: check nested inner defines only allowed at
           beginning of function. Peek last value on stack to check if last
           function is an `other_statement` before allowing next `define`. *)
        let rec check_inner_define value stack =
          match value with
          (* first inner define *)
          | A.Cons
              ( A.Id "lambda",
                A.Cons (A.Nil, A.Cons (A.Cons (A.Id "define", _), rest)) ) ->
              let new_stack = "define" :: stack in
              check_inner_define rest new_stack
          (* define *)
          | A.Cons (A.Cons (A.Id "define", _), rest) ->
              let hd = List.hd stack in
              if hd = "other_stmt" then
                raise
                  (Failure
                     ( "Nested inner defines only allowed at "
                     ^ "beginning of function" ))
              else
                let new_stack = "define" :: stack in
                check_inner_define rest new_stack
          (* other statements *)
          | A.Cons (_, rest) ->
              let new_stack = "other_stmt" :: stack in
              check_inner_define rest new_stack
          | _ -> None
        in
        let _ = check_inner_define value [] in

        (* support for recursive function *)
        let temp_symbol_table = Symtable.add name Any symbol_table in
        let value_type, _ = check_expr temp_symbol_table value in
        let new_symbol_table = Symtable.add name value_type symbol_table in
        let value_type, value = check_expr new_symbol_table value in
        (new_symbol_table, Value, Define (name, value))
    | A.Cons (A.Id "define", _) -> raise (Failure "Invalid define statement")
    | A.Cons (A.Id "set!", A.Cons (A.Id name, A.Cons (value, A.Nil))) ->
        if Symtable.mem name symbol_table then
          let value_type, value = check_expr symbol_table value in
          let new_symbol_table = Symtable.add name value_type symbol_table in
          (new_symbol_table, Value, Set (name, value))
        else raise (Failure ("Variable " ^ name ^ " is undefined"))
    | A.Cons (A.Id "set!", _) -> raise (Failure "Invalid set! statement")
    (* fallback to expr *)
    | expr ->
        let expr_type, expr = check_expr symbol_table expr in
        (symbol_table, expr_type, Expr expr)
  in

  let builtin_variables =
    Symtable.from
      [
        ("+", Function (Value, 2, 255));
        ("-", Function (Value, 2, 2));
        ("*", Function (Value, 2, 255));
        ("/", Function (Value, 2, 2));
        ("=", Function (Value, 2, 2));
        (">", Function (Value, 2, 2));
        ("<", Function (Value, 2, 2));
        (">=", Function (Value, 2, 2));
        ("<=", Function (Value, 2, 2));
        ("++", Function (Value, 2, 2));
        ("cons", Function (Value, 2, 2));
        ("car", Function (Value, 1, 1));
        ("cdr", Function (Value, 1, 1));
        ("list", Function (Value, 1, 255));
        ("display", Function (Value, 1, 255));
      ]
  in
  let _, sast =
    Utils.fold_map
      (fun st stmt ->
        let st, _, stmt = check_stmt st stmt in
        (st, stmt))
      builtin_variables expr_list
  in
      sast
