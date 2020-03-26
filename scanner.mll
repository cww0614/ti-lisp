{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf }
| eof                 { EOF }
| '.'                 { DOT }
| '\''                { QUOTE }
| '('                 { LEFT_BRACKET }
| ')'                 { RIGHT_BRACKET }
| ['0' - '9']+ as lit { INT_LITERAL(int_of_string lit) }
| ['a' - 'z' 'A' - 'Z' '0' - '9' '!' '@' '#' '$' '%' '^' '&' '*' '-' '_' '=' '+' '|' '\\' '/' '?' '<' '>' ':' '~']+ as id { IDENTIFIER(id) }

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "#"     { comment lexbuf }           (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
(* | '+'      { PLUS } *)
(* | '-'      { MINUS } *)
(* | '*'      { MULT } *)
(* | '/'      { DIVIDE } *)
| "define" { DEFINE }
| "lambda" { LAMBDA }
| "let"    { LET }
(* | "="     { EQ } *)
(* | "!="     { NEQ } *)
(* | '<'      { LT } *)
(* | '>'      { GT } *)
(* | "and"     { AND } *)
(* | "or"     { OR } *)
| "if"     { IF }
| "cond"   { COND }
| "true"   { BLIT(true)  }
| "false"  { BLIT(false) }
| digit+ as lem  { LITERAL(int_of_string lem) }
| letter (digit | letter | '_')* as lem { ID(lem) }
| ['=' '-' '*' '/' '>' '<' "<=" ">=" '=' "!="] as lem { ID(lem)}
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
(* TODO: Test if this actually only comments on a line basis *)
  ['\n' '\r'] { token lexbuf }
| _    { comment lexbuf }

