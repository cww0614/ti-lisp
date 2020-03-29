open Ast

let rec print_list f =  function
    | [] -> ""
    | hd::tl -> f hd ^ " " ^ print_list f tl


let rec print_expr = function
    | Literal(x) -> string_of_int x
    | BoolLit(x) -> string_of_bool x
    | Id(str) -> str
    | Call(op,expr_list) -> "Call(" ^ print_op op ^ ", ["^ print_list print_expr expr_list ^ "])"
    | BindList(bind_list,expr) -> "BindList([" ^print_list print_bind bind_list ^ "]," ^ print_expr expr ^ ")"
    | CondList(cond_list) -> "CondList([" ^ print_list print_cond cond_list ^ "])"
    | If (expr1,expr2,expr3) -> "If(" ^ print_expr expr1 ^ "," ^ print_expr expr2 ^ "," ^ print_expr expr3 ^ ")"
    | List(expr_list) -> "List([" ^ print_list print_expr expr_list ^ "])"

and print_op = function
    | Lambda(str_list,expr) -> "Lambda ([" ^ print_list (fun x -> x) str_list ^ "]," ^ print_expr expr ^ ")"
    | Fun(str) -> "Fun(" ^ str ^ ")"

and print_bind = function
    | Bind(str,expr) -> "Bind(" ^ str ^ "," ^ print_expr expr ^ ")"
and print_cond = function

    | Cond(exp1,exp2) -> "Cond(" ^ print_expr exp1 ^ "," ^ print_expr exp2 ^ ")"

let print_defn = function
    | DefineVar(str,expr) -> "DefineVar(" ^ str ^ "," ^ print_expr expr ^ ")"
    | DefineNewFun(str,str_list,expr) -> "DefineNewFun(" ^ str ^ ", [" ^ print_list (fun x -> x) str_list ^ "]," ^ print_expr expr ^ ")"

let print_stat = function
    | Exp (expr) -> print_expr expr
    | Define (defn) -> print_defn defn

let rec print_ast = function
    | [] -> ""
    | hd::tl -> print_stat hd ^ "\n" ^ print_ast tl


let _ =
  let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.program Scanner.token lexbuf in
  print_endline (print_ast expr)
