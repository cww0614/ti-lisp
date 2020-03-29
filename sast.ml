type stmt = Define of string * expr | Set of string * expr | Expr of expr

and expr =
  | CharLit of char
  | StrLit of string
  | IntLit of int
  | Id of string
  | Cons of expr * expr
  | Nil
  | Quote of expr
  | Begin of stmt list
  | Lambda of string list * stmt list
  | Let of (string * expr) list * stmt list
  | If of expr * expr * expr option
  | FunCall of expr * expr list

type program = stmt list

let add_indent str =
  let lines = String.split_on_char '\n' str in
  String.concat "\n" (List.map (fun x -> "  " ^ x) lines)

let rec string_of_stmt = function
  | Define (name, expr) -> "(define a " ^ string_of_expr expr ^ ")"
  | Set (name, expr) -> "(set! a " ^ string_of_expr expr ^ ")"
  | Expr expr -> string_of_expr expr

and string_of_expr = function
  | StrLit x -> "\"" ^ String.escaped x ^ "\""
  | IntLit x -> string_of_int x
  | CharLit x -> "'" ^ Char.escaped x ^ "'"
  | Id name -> name
  | Begin body ->
      "(begin\n"
      ^ add_indent (String.concat "\n" (List.map string_of_stmt body))
      ^ ")"
  | Lambda (args, body) ->
      "(lambda (" ^ String.concat " " args ^ ")\n"
      ^ add_indent (String.concat "\n" (List.map string_of_stmt body))
      ^ ")"
  | Let (bindings, body) ->
      "(let ("
      ^ String.concat "\n"
          (List.map
             (function
               | name, value -> "(" ^ name ^ " " ^ string_of_expr value ^ ")")
             bindings)
      ^ ")\n"
      ^ add_indent (String.concat "\n" (List.map string_of_stmt body))
      ^ ")"
  | FunCall (caller, args) ->
      "(" ^ string_of_expr caller ^ " "
      ^ String.concat " " (List.map string_of_expr args)
      ^ ")"
  | If (predicate, then_clause, maybe_else_clause) ->
      "(if " ^ string_of_expr predicate ^ "\n"
      ^ add_indent (string_of_expr then_clause)
      ^ "\n"
      ^ ( match maybe_else_clause with
        | Some else_clause -> add_indent (string_of_expr else_clause)
        | None -> "" )
      ^ ")"
  | _ ->
      raise
        (Failure
           "Programming Error: invalid expression should have been caught in \
            semantic analysis")

let string_of_stmt_block = List.map string_of_stmt
