type token =
  | LITERAL of (int)
  | BLITERAL of (bool)
  | ID of (string)
  | LPAREN
  | RPAREN
  | IF
  | COND
  | DEFINE
  | LAMBDA
  | LET
  | QUOTE
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
