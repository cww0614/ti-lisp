type expr =
| StrLit of string
| IntLit of int
| Id of string
| Cons of expr * expr
| Quote of expr
| Nil
