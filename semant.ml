(* Semantic checking for Lisp *)
open Ast
(* open Sast *)

module StringMap = Map.Make(String)

(* Semantic checking of AST. Returns SAST if success, throws an exception if
   something is wrong *)

let check (functions) =
   (functions)