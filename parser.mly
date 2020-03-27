%{ open Ast %}

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
  | obj {[Program($1)]}
  | obj program {Program($1)::$2}

obj:
    | expr {$1}
    | op {$1}

op:
    (* best way to define this to enable higher ordr functions? *)
    | LPAREN LAMBDA LPAREN defn_list RPAREN expr RPAREN {Lambda($4,$6)}
    | ID {Id($1)}

defn:
    | LPAREN DEFINE ID obj RPAREN {Define($3,$4)}
    | LPAREN DEFINE LPAREN ID defn_list RPAREN expr RPAREN {DefineFun($4,$5,$7)}

defn_list:
    | ID {[Id($1)]}
    | ID defn_list {Id($1)::$2}

expr:
    | LITERAL {Literal($1)}
    | BLITERAL {BoolLit($1)}
    | ID        {Id($1)}
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
    | LPAREN op args_opt RPAREN {Call($2,$3)}
    | LPAREN LET LPAREN bind_list RPAREN expr RPAREN {BindList($4,$6)}
    | LPAREN COND LPAREN cond_list RPAREN {CondList($4)}
    | LPAREN IF expr expr expr RPAREN {If($3,$4,$5)}
    | QUOTE LPAREN expr_list RPAREN {List($3)}

bind_list:
    | LPAREN ID expr RPAREN {[Bind($2,$3)]}
    | LPAREN ID expr RPAREN bind_list {Bind($2,$3)::$5}

cond_list:
    | LPAREN expr expr RPAREN {[Cond($2,$3)]}
    | LPAREN expr expr RPAREN cond_list {Cond($2,$3)::$5}

expr_list:
    | expr {[$1]}
    | expr expr_list {$1::$2}

args_opt:
    /*nothing*/ { []}
    | args_list {$1}

args_list:
    | expr { [$1]}
    | expr args_list {$1::$2}
