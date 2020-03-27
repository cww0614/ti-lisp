%{ open Ast %}




expr: LEFT_BRACKET list_body RIGHT_BRACKET { $2 }
  | INT_LITERAL                            { IntLit($1) }
  | QUOTE expr                             { Quote($2) }
  | IDENTIFIER                             { Id($1) }

list_body: expr list_body                  { Cons($1, $2) }
  | expr DOT expr                          { Cons($1, $3) }
  |                                        { Nil }

%token <int> LITERAL
%token <bool> BLIT
%token <string> ID
%token LPAREN RPAREN
%token IF COND
%token DEFINE LAMBDA LET
%token EOF


%start program
%type <Ast.program> program

%%
program:
  | obj_list EOF { $1}

obj_list:
  | obj
  | obj program
(* Q: Can we just define "and" and "or", "plus",etc... as built in functions rather than make them keywords? Ask TA? *)
(* Q: Should we separate expressions that return booleans? Scheme currently does not *)
(* Q: How to deal with quotation? *)

obj:
    | expr
    | op

op:
    (* best way to define this to enable higher ordr functions? *)
    | LPAREN LAMBDA LPAREN defn_list RPAREN expr RPAREN
    | ID

defn:
    | LPAREN DEFINE ID obj RPAREN
    | LPAREN DEFINE LPAREN ID defn_list RPAREN expr RPAREN

defn_list:
    | ID
    | ID defn_list

expr:
    | LITERAL
    | BLITERAL
    | ID
    (* | LPAREN PLUS expr expr RPAREN *)
    (* | LPARN MIUS expr expr RPAREN *)
    (* | LPAREN MULT expr expr RPAREN *)
    (* | LPAREN DIVIDE expr expr RPAREN *)
    (* | LPAREN EQ expr expr RPAREN *)
    (* | LPAREN NEQ expr expr RPAREN *)
    (* | LPAREN LT expr expr RPAREN *)
    (* | LPAREN GT expr expr RPAREN *)
    (* | LPAREN expr RPAREN *)
    (* Not just ID so we can allow lambda *)
    | LPAREN op args_opt RPAREN
    | LPAREN LET LPAREN bind_list RPAREN expr RPAREN
    | LPAREN COND LPAREN cond_list RPAREN
    | LPAREN IF expr expr expr RPAREN
    | QUOTE LPAREN expr_list RPAREN

bind_list:
    | LPAREN ID expr RPAREN
    | LPAREN ID expr RPAREN bind_list

cond_list:
    | LPAREN expr expr RPAREN
    | LPAREN expr expr RPAREN cond_list

expr_list:
    | expr
    | expr expr_list

args_opt:
    /*nothing*/
    | args_list

args_list:
    | expr
    | expr args_list
