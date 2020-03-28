type expr =
| CharLit of char
| StrLit of string
| IntLit of int
| Id of string
| Cons of expr * expr
| Quote of expr
| Nil
