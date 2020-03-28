{ open Parser }

let id_non_prefix_char = ['a' - 'z' 'A' - 'Z' '0' - '9' '!' '@' '$' '%' '^' '&' '*' '-' '_' '=' '+' '|' '\\' '/' '<' '>' ':' '~']
let id_char = (id_non_prefix_char | '?')

rule token = parse
  [' ' '\t' '\r' '\n']              { token lexbuf }
| eof                               { EOF }
| '#'                               { comment lexbuf }
| '.'                               { DOT }
| '\''                              { QUOTE }
| '('                               { LEFT_BRACKET }
| ')'                               { RIGHT_BRACKET }
| '"'                               { read_string (Buffer.create 17) lexbuf }
(* ' is already used by quote *)
| '?'                               { read_char lexbuf }
| ['0' - '9']+ as lit               { INT_LITERAL(int_of_string lit) }
| id_non_prefix_char id_char* as id { IDENTIFIER(id) }
| _ as char                         { raise (Failure("illegal character " ^ Char.escaped char)) }

(* Adpated from https://dev.realworldocaml.org/parsing-with-ocamllex-and-menhir.html *)
and read_string buf = parse
| '"'                { STR_LITERAL (Buffer.contents buf) }
| '\\' '/'           { Buffer.add_char buf '/'; read_string buf lexbuf }
| '\\' '\\'          { Buffer.add_char buf '\\'; read_string buf lexbuf }
| '\\' 'b'           { Buffer.add_char buf '\b'; read_string buf lexbuf }
| '\\' 'f'           { Buffer.add_char buf '\012'; read_string buf lexbuf }
| '\\' 'n'           { Buffer.add_char buf '\n'; read_string buf lexbuf }
| '\\' 'r'           { Buffer.add_char buf '\r'; read_string buf lexbuf }
| '\\' 't'           { Buffer.add_char buf '\t'; read_string buf lexbuf }
| [^ '"' '\\']+ as s { Buffer.add_string buf s; read_string buf lexbuf }
| _ as char          { raise (Failure ("Illegal string character: " ^ Char.escaped char)) }
| eof                { raise (Failure ("String is not terminated")) }

and read_char = parse
| '\\' '/'  { CHAR_LITERAL '/' }
| '\\' '\\' { CHAR_LITERAL '\\' }
| '\\' 'b'  { CHAR_LITERAL '\b' }
| '\\' 'f'  { CHAR_LITERAL '\012' }
| '\\' 'n'  { CHAR_LITERAL '\n' }
| '\\' 'r'  { CHAR_LITERAL '\r' }
| '\\' 't'  { CHAR_LITERAL '\t' }
| _  as c   { CHAR_LITERAL c }
| eof       { raise (Failure ("Char is not terminated")) }

and comment = parse
 ['\n' '\r'] { token lexbuf }
| _ { comment lexbuf }
