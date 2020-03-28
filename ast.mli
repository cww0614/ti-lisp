type stat =
    Exp of expr
    | Define of defn

type bind = Bind of string * expr

type op =
    Lambda of string list * expr
    | Fun of string

type cond = Cond of expr * expr

type defn =
    DefineVar of string * expr
    | DefineFun of string * op
    | DefineNewFun of string * string list * expr

type expr =
    Literal of int
  | BoolLit of bool
  | Id of string
  | Call of op * expr list
  | BindList of bind list * expr
  | CondList of cond list
  | If of expr * expr * expr
  | List expr list
