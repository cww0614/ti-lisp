{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf }
| eof                  { EOF }
| '#'                  { comment lexbuf }
| '.'                  { DOT }
| '\''                 { QUOTE }
| '('                  { LEFT_BRACKET }
| ')'                  { RIGHT_BRACKET }
| ['0' - '9']+ as lit  { INT_LITERAL(int_of_string lit) }
| ['a' - 'z' 'A' - 'Z' '0' - '9' '!' '@' '$' '%' '^' '&' '*' '-' '_' '=' '+' '|' '\\' '/' '?' '<' '>' ':' '~']+ as id { IDENTIFIER(id) }

and comment = parse
 ['\n' '\r'] { token lexbuf }
| _ { comment lexbuf }
