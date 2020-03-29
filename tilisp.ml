open Ast

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  let expanded = Macro.expand_all program in
  List.iter (fun x -> print_endline (Ast.string_of_ast x)) expanded
