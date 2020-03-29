type expr =
    Literal of int
  | BoolLit of bool
  | Id of string
  | Call of op * expr list
  | BindList of bind list * expr
  | CondList of cond list
  | If of expr * expr * expr
  | List of expr list
and op =
    Lambda of string list * expr
    | Fun of string
and bind = Bind of string * expr
and cond = Cond of expr * expr

type defn =
    DefineVar of string * expr
    | DefineFun of string * op
    | DefineNewFun of string * string list * expr

type stat =
    Exp of expr
    | Define of defn

type program = stat list
