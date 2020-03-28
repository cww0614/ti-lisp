open Ast

let rec print_ast = function
  | StrLit (x)  -> "\"" ^ String.escaped x ^ "\""
  | IntLit (x)  -> string_of_int x
  | CharLit (x)  -> "'" ^ Char.escaped x  ^"'"
  | Id (name)   -> name
  | Quote (v)   -> "Quote [" ^ print_ast v ^ "]"
  | Cons (a, b) -> "Cons ["
                   ^ print_ast a
                   ^ ", " ^ print_ast b
                   ^ "]"
  | Nil         -> "Nil"

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  List.iter (fun x -> print_endline (print_ast x)) program
