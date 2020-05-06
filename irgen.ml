module L = Llvm
module A = Ast
open Sast

let translate () =
  let context = L.global_context () in
  let the_module = L.create_module context "ti-lisp" in

  let i1_t = L.i8_type context in
  let i8_t = L.i8_type context in
  let i32_t = L.i32_type context in
  let i64_t = L.i32_type context in

  let i8_ptr_t = L.pointer_type i8_t in

  (* The union is represented by a 16-byte char array (Assuming each
     pointer is 8 bytes here). *)
  let value_type = L.struct_type context [| i8_t; L.array_type i8_t 16 |] in
  let value_ptr_type = L.pointer_type value_type in

  let type_integer = 0
  and type_char = 1
  and type_string = 2
  and type_cons = 3
  and type_bool = 4
  and type_nil = 5
  and type_func = 6 in

  (* When using a value as specific type, the value_type will be
     bitcasted to one of theses types *)
  let value_type_int = L.struct_type context [| i8_t; i32_t |] in
  let value_type_char = L.struct_type context [| i8_t; i8_t |] in
  let value_type_bool = L.struct_type context [| i8_t; i1_t |] in
  let value_type_string = L.struct_type context [| i8_t; i8_ptr_t; i64_t |] in
  let value_type_cons =
    L.struct_type context [| i8_t; value_ptr_type; value_ptr_type |]
  in
  let value_type_func =
    L.struct_type context [| i8_t; i8_ptr_t; i8_t; i8_t |]
  in

  the_module
