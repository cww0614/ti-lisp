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
    /* nothing */ {[]}
  | stat {[$1]}
  | stat program {$1::$2}

stat:
    | expr { Exp($1)}
    | defn { Define($1)} 


op:
    | LPAREN LAMBDA LPAREN defn_list RPAREN expr RPAREN {Lambda($4,$6)}
    | ID {Fun($1)}

    
defn:
    | LPAREN DEFINE ID expr RPAREN {DefineVar($3,$4)}
    | LPAREN DEFINE ID op RPAREN {DefineFun($3,$4)}
    | LPAREN DEFINE LPAREN ID defn_list RPAREN expr RPAREN {DefineNewFun($4,$5,$7)}

defn_list:
    | ID {[$1]}
    | ID defn_list {$1::$2}

expr:
    | LITERAL {Literal($1)}
    | BLITERAL {BoolLit($1)}
    | ID        {Id($1)}
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
