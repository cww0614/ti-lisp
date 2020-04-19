type stmt = Define of string * expr | Set of string * expr | Expr of expr

and expr =
  | CharLit of char
  | StrLit of string
  | IntLit of int
  | Id of string
  (* lists/cons: '(1 2 3 ...) *)
  | Cons of expr * expr
  (* '() *)
  | Nil
  (* 'symbol-name *)
  | Symbol of string
  (* (begin stmt1 stmt2 ...) *)
  | Begin of stmt list
  (* (lambda (arg1 arg2 ...) stmt1 ...) *)
  | Lambda of string list * stmt list
  (* (let ((name value)) body ...)  *)
  | Let of (string * expr) list * stmt list
  (* (if test then else), the else is optional here *)
  | If of expr * expr * expr option
  (* (fun arg1 arg2 ...) *)
  | FunCall of expr * expr list

type program = stmt list

(* Add indentation to each line in the string, useful for pretty
   printing the sast.

   Example: "a\nb\nc" => " a\n b\n c" *)
let add_indent (str : string) : string =
  let lines = String.split_on_char '\n' str in
  String.concat "\n" (List.map (fun x -> "  " ^ x) lines)

(* Convert stmt to string for inspection *)
let rec string_of_stmt : stmt -> string = function
  | Define (name, expr) -> "(define " ^ name ^ " " ^ string_of_expr expr ^ ")"
  | Set (name, expr) -> "(set! " ^ name ^ " " ^ string_of_expr expr ^ ")"
  | Expr expr -> string_of_expr expr

(* Convert expr to string for inspection *)
and string_of_expr : expr -> string = function
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
      ^ String.concat " "
          (List.map
             (function
               | name, value -> "(" ^ name ^ " " ^ string_of_expr value ^ ")")
             bindings)
      ^ ")\n"
      ^ add_indent (String.concat "\n" (List.map string_of_stmt body))
      ^ ")"
  | FunCall (func, args) ->
      "(funcall " ^ string_of_expr func ^ " "
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
  | Symbol name -> "'" ^ name
  | Cons (hd, tl) ->
      "(cons " ^ string_of_expr hd ^ " " ^ string_of_expr tl ^ ")"
  | Nil -> "nil"

(* Convert list of stmt to list of string for inspection *)
let string_of_stmt_block : stmt list -> string list = List.map string_of_stmt
