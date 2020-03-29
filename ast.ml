type expr =
  | CharLit of char
  | StrLit of string
  | IntLit of int
  | Id of string
  | Cons of expr * expr
  | Quote of expr
  | Expansion
  | Nil

let rec string_of_ast = function
  | StrLit x -> "\"" ^ String.escaped x ^ "\""
  | IntLit x -> string_of_int x
  | CharLit x -> "'" ^ Char.escaped x ^ "'"
  | Id name -> name
  | Quote v -> "Quote [" ^ string_of_ast v ^ "]"
  | Cons (a, b) -> "Cons [" ^ string_of_ast a ^ ", " ^ string_of_ast b ^ "]"
  | Expansion -> "..."
  | Nil -> "Nil"

let rec cons_to_list = function
  | Cons (a, b) -> a :: cons_to_list b
  | Nil -> []
  | _ -> raise (Failure "cons_to_list called on non-list")
