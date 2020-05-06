module L = Llvm
module A = Ast
open Sast

(* References:

   - https://llvm.moe/ocaml/Llvm.html
   - https://mapping-high-level-constructs-to-llvm-ir.readthedocs.io/en/latest/basic-constructs/unions.html
 *)

type symbol_table = L.llvalue Symtable.symbol_table

let translate (stmts : stmt list) =
  let context = L.global_context () in
  let the_module = L.create_module context "ti-lisp" in

  let i1_t = L.i8_type context in
  let i8_t = L.i8_type context in
  let i32_t = L.i32_type context in
  let i64_t = L.i32_type context in

  let i8_ptr_t = L.pointer_type i8_t in

  let void_t = L.void_type context in

  let create_struct name ctx body =
    let struct_ = L.named_struct_type ctx name in
    L.struct_set_body struct_ body false;
    struct_
  in

  (* The union is represented by a 16-byte char array (Assuming each
     pointer is 8 bytes here). *)
  let value_type =
    create_struct "value_t" context [| i8_t; L.array_type i8_t 16 |]
  in
  let value_ptr_type = L.pointer_type value_type in
  let value_size = 24 in

  let type_integer = L.const_int i8_t 0
  and _type_char = L.const_int i8_t 1
  and _type_string = L.const_int i8_t 2
  and _type_cons = L.const_int i8_t 3
  and _type_bool = L.const_int i8_t 4
  and _type_nil = L.const_int i8_t 5
  and _type_func = L.const_int i8_t 6 in

  (* When using a value as specific type, the value_type will be
     bitcasted to one of theses types *)
  let value_type_int = create_struct "value_t_int" context [| i8_t; i32_t |] in
  let _value_type_char =
    create_struct "value_t_char" context [| i8_t; i8_t |]
  in
  let _value_type_bool =
    create_struct "value_t_bool" context [| i8_t; i1_t |]
  in
  let _value_type_string =
    create_struct "value_t_string" context [| i8_t; i8_ptr_t; i64_t |]
  in
  let _value_type_cons =
    create_struct "value_t_cons" context
      [| i8_t; value_ptr_type; value_ptr_type |]
  in
  let _value_type_func =
    create_struct "value_t_func" context [| i8_t; i8_ptr_t; i8_t; i8_t |]
  in

  let build_memcpy src dest builder =
    let memcpy_func =
      let func_type =
        L.function_type void_t [| i8_ptr_t; i8_ptr_t; i32_t; i1_t |]
      in
      L.declare_function "llvm.memcpy" func_type the_module
    in

    let dest = L.build_bitcast dest i8_ptr_t "memcpy_dest" builder in
    let src = L.build_bitcast src i8_ptr_t "memcpy_src" builder in
    L.build_call memcpy_func
      [| dest; src; L.const_int i32_t value_size; L.const_int i1_t 0 |]
      "" builder
  in

  let rec build_stmt (st : symbol_table) (builder : L.llbuilder) :
      stmt -> symbol_table = function
    | Define (name, value) ->
        let value = build_expr name st builder value in
        Symtable.add name value st
    | Set (name, value) ->
        let value = build_unnamed_expr st builder value in
        ignore
          ( match Symtable.find name st with
          | Some pos -> build_memcpy value pos builder
          | _ -> raise (Failure "Undefined variable") );
        st
    | _ -> raise (Failure "Not implemented")
  and build_unnamed_expr st builder = build_expr "unnamed" st builder
  and build_expr (name : string) (st : symbol_table) (builder : L.llbuilder) :
      expr -> L.llvalue =
    let build_literal type_value ltype values =
      let alloca = L.build_alloca value_type name builder in
      let type_field = L.build_struct_gep alloca 0 "value_type" builder in
      ignore (L.build_store type_value type_field builder);
      let casted =
        L.build_bitcast alloca (L.pointer_type ltype) "content" builder
      in
      List.iter
        (function
          | idx, value ->
              let field = L.build_struct_gep casted idx "field" builder in
              ignore (L.build_store value field builder))
        values;
      alloca
    in

    function
    | IntLit v ->
        build_literal type_integer value_type_int [ (1, L.const_int i32_t v) ]
    | _ -> raise (Failure "Not implemented")
  in

  let main_func =
    let main_func_type = L.function_type i32_t [||] in
    L.define_function "main" main_func_type the_module
  in

  let builder = L.builder_at_end context (L.entry_block main_func) in

  ignore
    (List.fold_left
       (fun st stmt -> build_stmt st builder stmt)
       (Symtable.from []) stmts);
  ignore (L.build_ret (L.const_int i32_t 0) builder);
  the_module
