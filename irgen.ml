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

  let i1_t = L.i1_type context in
  let i8_t = L.i8_type context in
  let i32_t = L.i32_type context in
  let i64_t = L.i64_type context in

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
  and type_bool = L.const_int i8_t 4
  and _type_func = L.const_int i8_t 5 in

  (* When using a value as specific type, the value_type will be
     bitcasted to one of theses types *)
  let value_type_int = create_struct "value_t_int" context [| i8_t; i64_t |] in
  let _value_type_char =
    create_struct "value_t_char" context [| i8_t; i8_t |]
  in
  let value_type_bool = create_struct "value_t_bool" context [| i8_t; i1_t |] in
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
  let display_func : L.llvalue =
    let display_t = L.function_type value_ptr_type [| value_ptr_type |] in
    L.declare_function "display" display_t the_module
  in

  let check_type_func : L.llvalue =
    let func_type = L.function_type void_t [| value_ptr_type; i8_t |] in
    L.declare_function "check_type" func_type the_module
  in

  let build_literal name type_value ltype values builder =
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

  let build_memcpy src dest builder =
    let memcpy_func =
      let func_type =
        L.function_type void_t [| i8_ptr_t; i8_ptr_t; i64_t; i1_t |]
      in
      L.declare_function "llvm.memcpy.p0i8.p0i8.i64" func_type the_module
    in

    let dest = L.build_bitcast dest i8_ptr_t "memcpy_dest" builder in
    let src = L.build_bitcast src i8_ptr_t "memcpy_src" builder in
    L.build_call memcpy_func
      [| dest; src; L.const_int i64_t value_size; L.const_int i1_t 0 |]
      "" builder
  in

  let add_terminal builder instr =
    match L.block_terminator (L.insertion_block builder) with
    | Some _ -> ()
    | None -> ignore (instr builder)
  in

  let rec build_stmt (func : L.llvalue) (st : symbol_table)
      (builder : L.llbuilder) : stmt -> symbol_table * L.llbuilder = function
    | Define (name, value) ->
        let _, value = build_expr func name st builder value in
        (Symtable.add name value st, builder)
    | Set (name, value) ->
        let builder, value = build_unnamed_expr func st builder value in
        ignore
          ( match Symtable.find name st with
          | Some pos -> build_memcpy value pos builder
          | _ -> raise (Failure "Undefined variable") );
        (st, builder)
    | Expr expr ->
        let builder, _ = build_unnamed_expr func st builder expr in
        (st, builder)
  and build_unnamed_expr func st builder = build_expr func "unnamed" st builder
  and build_expr (func : L.llvalue) (name : string) (st : symbol_table)
      (builder : L.llbuilder) : expr -> L.llbuilder * L.llvalue = function
    | IntLit v ->
        ( builder,
          build_literal name type_integer value_type_int
            [ (1, L.const_int i64_t v) ]
            builder )
    | BoolLit v ->
        ( builder,
          build_literal name type_bool value_type_bool
            [ (1, L.const_int i1_t (if v then 1 else 0)) ]
            builder )
    | Id name -> (
        match Symtable.find name st with
        | Some value -> (builder, value)
        | None -> raise (Failure "Undefined variable") )
    | If (pred, then_c, else_c) ->
        let builder, pred_val = build_expr func "cmp" st builder pred in
        let casted =
          L.build_bitcast pred_val
            (L.pointer_type value_type_bool)
            "bool_type_val" builder
        in
        let value_ptr = L.build_struct_gep casted 1 "bool_val_ptr" builder in
        let value = L.build_load value_ptr "bool_val" builder in
        let orig_builder = builder in
        ignore
          (L.build_call check_type_func [| pred_val; type_bool |] "" builder);
        let then_bb = L.append_block context "then" func in
        let builder, then_value =
          build_expr func "then" st (L.builder_at_end context then_bb) then_c
        in
        let else_bb = L.append_block context "else" func in
        let else_value =
          match else_c with
          | Some expr ->
              let _, value =
                build_expr func "else" st
                  (L.builder_at_end context else_bb)
                  expr
              in
              value
          | None -> L.const_null value_ptr_type
        in

        let end_bb = L.append_block context "end" func in
        let build_br_end = L.build_br end_bb in
        add_terminal (L.builder_at_end context then_bb) build_br_end;
        add_terminal (L.builder_at_end context else_bb) build_br_end;
        ignore (L.build_cond_br value then_bb else_bb orig_builder);
        let builder = L.builder_at_end context end_bb in
        let phi =
          L.build_phi
            [ (then_value, then_bb); (else_value, else_bb) ]
            "ifval" builder
        in
        (builder, phi)
    | FunCall (func, args) -> (
        match func with
        | Id name -> (
            match Symtable.find name st with
            | Some func -> (
                (* Reference: https://groups.google.com/forum/#!topic/llvm-dev/_xy_3ZpQFLI *)
                match L.classify_type (L.type_of func) with
                (* TODO: check function argument number here *)
                (* TODO: User defined functions *)
                (* Builtin functions *)
                | L.TypeKind.Pointer ->
                    let builder, args =
                      Utils.fold_map (build_unnamed_expr func st) builder args
                    in
                    let args = Array.of_list args in
                    let ret = L.build_call func args "ret" builder in
                    (builder, ret)
                | _ -> raise (Failure "Unexpected function type") )
            | None -> raise (Failure ("Function " ^ name ^ " is not defined")) )
        | _ -> raise (Failure "Not implemented") )
    | _ -> raise (Failure "Not implemented")
  in

  let main_func =
    let main_func_type = L.function_type i32_t [||] in
    L.define_function "main" main_func_type the_module
  in

  let builder = L.builder_at_end context (L.entry_block main_func) in

  let _, builder =
    List.fold_left
      (fun ctx stmt ->
        let st, builder = ctx in
        build_stmt main_func st builder stmt)
      (Symtable.from [ ("display", display_func) ], builder)
      stmts
  in

  ignore (L.build_ret (L.const_int i32_t 0) builder);
  the_module
