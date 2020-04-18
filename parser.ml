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

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"
 open Ast 
# 20 "parser.ml"
let yytransl_const = [|
  260 (* LPAREN *);
  261 (* RPAREN *);
  262 (* IF *);
  263 (* COND *);
  264 (* DEFINE *);
  265 (* LAMBDA *);
  266 (* LET *);
  267 (* QUOTE *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* LITERAL *);
  258 (* BLITERAL *);
  259 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\003\000\003\000\006\000\006\000\
\005\000\005\000\007\000\007\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\009\000\009\000\010\000\010\000\
\011\000\011\000\008\000\008\000\012\000\012\000\000\000"

let yylen = "\002\000\
\001\000\002\000\001\000\002\000\001\000\001\000\007\000\001\000\
\005\000\008\000\001\000\002\000\001\000\001\000\001\000\004\000\
\007\000\005\000\006\000\004\000\004\000\005\000\004\000\005\000\
\001\000\002\000\000\000\001\000\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\013\000\014\000\015\000\000\000\000\000\001\000\
\031\000\000\000\000\000\005\000\006\000\008\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\002\000\004\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\028\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\030\000\016\000\026\000\020\000\000\000\
\000\000\000\000\000\000\018\000\009\000\000\000\000\000\000\000\
\012\000\000\000\019\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\017\000\007\000\024\000\010\000\022\000"

let yydgoto = "\002\000\
\009\000\010\000\011\000\012\000\013\000\020\000\049\000\032\000\
\043\000\039\000\035\000\033\000"

let yysindex = "\010\000\
\001\000\000\000\000\000\000\000\000\000\024\255\010\255\000\000\
\000\000\017\000\005\255\000\000\000\000\000\000\020\255\022\255\
\015\255\255\254\032\255\022\255\022\255\000\000\000\000\033\255\
\040\255\022\255\034\255\022\255\038\255\044\255\022\255\046\255\
\000\000\022\255\047\255\050\255\022\255\022\255\049\255\051\255\
\050\255\052\255\053\255\000\000\000\000\000\000\000\000\050\255\
\054\255\055\255\022\255\000\000\000\000\056\255\022\255\022\255\
\000\000\022\255\000\000\057\255\022\255\058\255\059\255\060\255\
\034\255\061\255\044\255\000\000\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\049\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\062\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\063\255\000\000\
\000\000\064\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\065\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\066\255\000\000\067\255\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\046\000\000\000\240\255\000\000\000\000\228\255\000\000\
\006\000\009\000\041\000\045\000"

let yytablesize = 268
let yytable = "\026\000\
\008\000\028\000\029\000\031\000\034\000\003\000\004\000\005\000\
\006\000\037\000\001\000\040\000\054\000\021\000\031\000\007\000\
\022\000\034\000\027\000\057\000\050\000\051\000\003\000\004\000\
\005\000\025\000\014\000\015\000\024\000\016\000\017\000\018\000\
\007\000\019\000\060\000\030\000\036\000\038\000\062\000\063\000\
\041\000\064\000\014\000\015\000\066\000\016\000\017\000\042\000\
\003\000\019\000\045\000\047\000\048\000\052\000\055\000\053\000\
\023\000\056\000\058\000\059\000\061\000\065\000\067\000\068\000\
\069\000\071\000\027\000\029\000\025\000\011\000\023\000\021\000\
\072\000\070\000\046\000\044\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\003\000\004\000\005\000\006\000\000\000\000\000\000\000\
\000\000\000\000\000\000\007\000"

let yycheck = "\016\000\
\000\000\003\001\004\001\020\000\021\000\001\001\002\001\003\001\
\004\001\026\000\001\000\028\000\041\000\004\001\031\000\011\001\
\000\000\034\000\004\001\048\000\037\000\038\000\001\001\002\001\
\003\001\004\001\003\001\004\001\009\001\006\001\007\001\008\001\
\011\001\010\001\051\000\004\001\004\001\004\001\055\000\056\000\
\003\001\058\000\003\001\004\001\061\000\006\001\007\001\004\001\
\000\000\010\001\005\001\005\001\003\001\005\001\003\001\005\001\
\011\000\005\001\005\001\005\001\005\001\005\001\005\001\005\001\
\005\001\005\001\005\001\005\001\005\001\005\001\005\001\005\001\
\067\000\065\000\034\000\031\000\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\002\001\003\001\004\001\255\255\255\255\255\255\
\255\255\255\255\255\255\011\001"

let yynames_const = "\
  LPAREN\000\
  RPAREN\000\
  IF\000\
  COND\000\
  DEFINE\000\
  LAMBDA\000\
  LET\000\
  QUOTE\000\
  EOF\000\
  "

let yynames_block = "\
  LITERAL\000\
  BLITERAL\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    Obj.repr(
# 17 "parser.mly"
                      ([])
# 189 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'obj_list) in
    Obj.repr(
# 18 "parser.mly"
                 ( _1)
# 196 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stat) in
    Obj.repr(
# 21 "parser.mly"
         ([_1])
# 203 "parser.ml"
               : 'obj_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stat) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'obj_list) in
    Obj.repr(
# 22 "parser.mly"
                  (_1::_2)
# 211 "parser.ml"
               : 'obj_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 25 "parser.mly"
           ( Exp(_1))
# 218 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'defn) in
    Obj.repr(
# 26 "parser.mly"
           ( Define(_1))
# 225 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'defn_list) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 30 "parser.mly"
                                                        (Lambda(_4,_6))
# 233 "parser.ml"
               : 'op))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 31 "parser.mly"
         (Fun(_1))
# 240 "parser.ml"
               : 'op))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 35 "parser.mly"
                                   (DefineVar(_3,_4))
# 248 "parser.ml"
               : 'defn))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : 'defn_list) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 36 "parser.mly"
                                                           (DefineNewFun(_4,_5,_7))
# 257 "parser.ml"
               : 'defn))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 39 "parser.mly"
         ([_1])
# 264 "parser.ml"
               : 'defn_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'defn_list) in
    Obj.repr(
# 40 "parser.mly"
                   (_1::_2)
# 272 "parser.ml"
               : 'defn_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 43 "parser.mly"
              (Literal(_1))
# 279 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 44 "parser.mly"
               (BoolLit(_1))
# 286 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 45 "parser.mly"
                (Id(_1))
# 293 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'op) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 46 "parser.mly"
                                (Call(_2,_3))
# 301 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'bind_list) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 47 "parser.mly"
                                                     (BindList(_4,_6))
# 309 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'cond_list) in
    Obj.repr(
# 48 "parser.mly"
                                          (CondList(_4))
# 316 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 49 "parser.mly"
                                      (If(_3,_4,_5))
# 325 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr_list) in
    Obj.repr(
# 50 "parser.mly"
                                    (List(_3))
# 332 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 53 "parser.mly"
                            ([Bind(_2,_3)])
# 340 "parser.ml"
               : 'bind_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'bind_list) in
    Obj.repr(
# 54 "parser.mly"
                                      (Bind(_2,_3)::_5)
# 349 "parser.ml"
               : 'bind_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 57 "parser.mly"
                              ([Cond(_2,_3)])
# 357 "parser.ml"
               : 'cond_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'cond_list) in
    Obj.repr(
# 58 "parser.mly"
                                        (Cond(_2,_3)::_5)
# 366 "parser.ml"
               : 'cond_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 61 "parser.mly"
           ([_1])
# 373 "parser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr_list) in
    Obj.repr(
# 62 "parser.mly"
                     (_1::_2)
# 381 "parser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 65 "parser.mly"
                ( [])
# 387 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args_list) in
    Obj.repr(
# 66 "parser.mly"
                (_1)
# 394 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 69 "parser.mly"
           ( [_1])
# 401 "parser.ml"
               : 'args_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'args_list) in
    Obj.repr(
# 70 "parser.mly"
                     (_1::_2)
# 409 "parser.ml"
               : 'args_list))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
