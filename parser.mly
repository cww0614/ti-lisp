%{ open Ast %}

%token EOF LEFT_BRACKET RIGHT_BRACKET DOT QUOTE
%token <int> INT_LITERAL
%token <string> STR_LITERAL
%token <char> CHAR_LITERAL
%token <string> IDENTIFIER

%left LEFT_BRACKET RIGHT_BRACKET

%start expr
%type <Ast.expr> expr
%type <Ast.expr> list_body

%%

expr: LEFT_BRACKET list_body RIGHT_BRACKET { $2 }
  | INT_LITERAL                            { IntLit($1) }
  | STR_LITERAL                            { StrLit($1) }
  | CHAR_LITERAL                           { CharLit($1) }
  | QUOTE expr                             { Quote($2) }
  | IDENTIFIER                             { Id($1) }

list_body: expr list_body                  { Cons($1, $2) }
  | expr DOT expr                          { Cons($1, $3) }
  |                                        { Nil }
