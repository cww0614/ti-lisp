module L = Llvm
module A = Ast
open Sast

(* References:

   - https://llvm.moe/ocaml/Llvm.html
   - https://mapping-high-level-constructs-to-llvm-ir.readthedocs.io/en/latest/basic-constructs/unions.html
 *)

type value_t = { value : L.llvalue; real_func : L.llvalue option }

let make_val v = { value = v; real_func = None }

let llvalue_of = function { value } -> value

let copy_value_metadata src dest =
  let { value = dest_value } = dest in
  { src with value = dest_value }

type symbol_table = value_t Symtable.symbol_table

let translate (stmts : stmt list) =
  let context = L.global_context () in
  let the_module = L.create_module context "ti-lisp" in

  let i1_t = L.i1_type context in
  let i8_t = L.i8_type context in
  let i32_t = L.i32_type context in
  let i64_t = L.i64_type context in
  let f64_t = L.double_type context in

  let i8_ptr_t = L.pointer_type i8_t in

  let void_t = L.void_type context in

  let create_struct name ctx body =
    let struct_ = L.named_struct_type ctx name in
    L.struct_set_body struct_ body false;
    struct_
  in

  (* The union is represented by a 24-byte char array (Assuming each
     pointer is 8 bytes here). *)
  let value_type =
    create_struct "value_t" context [| i64_t; L.array_type i8_t 24 |]
  in
  let value_ptr_type = L.pointer_type value_type in
  let value_size = 32 in

  let type_integer = L.const_int i64_t 0
  and type_char = L.const_int i64_t 1
  and type_string = L.const_int i64_t 2
  and type_cons = L.const_int i64_t 3
  and type_bool = L.const_int i64_t 4
  and type_func = L.const_int i64_t 5
  and type_symbol = L.const_int i64_t 6 
  and type_float = L.const_int i64_t 7 in

  (* When using a value as specific type, the value_type will be
     bitcasted to one of theses types *)
  let value_type_int = create_struct "value_t_int" context [| i64_t; i64_t |] in
  let value_type_real = create_struct "value_t_real" context [| i64_t; f64_t |] in
  let value_type_char =
    create_struct "value_t_char" context [| i64_t; i8_t |]
  in
  let value_type_bool =
    create_struct "value_t_bool" context [| i64_t; i1_t |]
  in
  let value_type_string =
    create_struct "value_t_string" context [| i64_t; i8_ptr_t; i64_t |]
  in
  let value_type_cons =
    create_struct "value_t_cons" context
      [| i64_t; value_ptr_type; value_ptr_type |]
  in
  let value_type_func =
    create_struct "value_t_func" context
      [| i64_t; i8_ptr_t; i8_ptr_t; i8_t; i8_t |]
  in
  let value_type_symbol =
    create_struct "value_t_string" context [| i64_t; i8_ptr_t; i64_t |]
  in

  let check_type_func : L.llvalue =
    let func_type = L.function_type void_t [| value_ptr_type; i64_t |] in
    L.declare_function "check_type" func_type the_module
  in

  let check_func_func : L.llvalue =
    let func_type = L.function_type void_t [| value_ptr_type; i8_t |] in
    L.declare_function "check_func" func_type the_module
  in

  let build_malloc tp name builder = L.build_malloc tp name builder in

  let build_literal alloca type_value ltype values builder =
    (* type_field is the tag in the llvm struct member*)
    let type_field = L.build_struct_gep alloca 0 "value_type" builder in
    ignore (L.build_store type_value type_field builder);
    let casted =
      (* recasting alloca struct*)
      L.build_bitcast alloca (L.pointer_type ltype) "content" builder
    in
    (* writing the value to the struct, which is wrapped inside the "values" tuple *)
    List.iter
      (function
        | idx, value ->
            let field = L.build_struct_gep casted idx "field" builder in
            ignore (L.build_store value field builder))
      values
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
    ignore
      (L.build_call memcpy_func
         [| dest; src; L.const_int i64_t value_size; L.const_int i1_t 0 |]
         "" builder)
  in

  let add_terminal builder instr =
    match L.block_terminator (L.insertion_block builder) with
    | Some _ -> ()
    | None -> ignore (instr builder)
  in

  let function_type (arg_size : int) : L.lltype =
    let args_types = Array.make (1 + arg_size) value_ptr_type in
    let access_link_type = i8_ptr_t in
    args_types.(0) <- access_link_type;
    L.function_type value_ptr_type args_types
  in

  let builtins =
    Symtable.from
      (List.map
         (fun spec ->
           let lisp_name, cpp_name, min_arg, max_arg = spec in
           let func_type =
             if min_arg = max_arg then function_type min_arg
             else
               L.var_arg_function_type value_ptr_type
                 (Array.make min_arg value_ptr_type)
           in
           let func = L.declare_function cpp_name func_type the_module in
           (lisp_name, make_val func))
         [
           ("display", "display", 1, 1);
           ("+", "cpp_add", 2, 2);
           ("-", "cpp_subtract", 2, 2);
           ("*", "cpp_mult", 2, 2);
           ("/", "cpp_div", 2, 2);
           ("=", "cpp_equal", 2, 2);
           ("<", "cpp_less_than", 2, 2);
           (">", "cpp_more_than", 2, 2);
           ("<=", "cpp_leq", 2, 2);
           (">=", "cpp_geq", 2, 2);
           ("++", "cpp_concat", 2, 2);
           ("integer?", "is_integer", 1, 1);
           ("float?", "is_float", 1, 1);
           ("char?", "is_char", 1, 1);
           ("string?", "is_string", 1, 1);
           ("cons?", "is_cons", 1, 1);
           ("bool?", "is_bool", 1, 1);
           ("function?", "is_function", 1, 1);
           ("symbol?", "is_symbol", 1, 1);
           ("nil?", "is_nil", 1, 1);
           ("cons", "cpp_cons", 2, 2);
           ("car", "cpp_car", 1, 1);
           ("cdr", "cpp_cdr", 1, 1);
         ])
  in

  (* Find which variables in outer function are used *)
  let rec collect_dependency (stmts : stmt list) (args : string list) :
      string list =
    let rec collect_dep_stmts (st : unit Symtable.symbol_table)
        (outer_vars : string list) (stmts : stmt list) :
        unit Symtable.symbol_table * string list =
      List.fold_left
        (fun ctx stmt ->
          let st, outer_vars = ctx in
          collect_dep_stmt st outer_vars stmt)
        (st, outer_vars) stmts
    and collect_dep_stmt (st : unit Symtable.symbol_table)
        (outer_vars : string list) :
        stmt -> unit Symtable.symbol_table * string list = function
      | Define (name, value) ->
          let st = Symtable.add name () st in
          (st, collect_dep_expr st outer_vars value)
      | Set (name, value) ->
          if Symtable.mem name st then (st, collect_dep_expr st outer_vars value)
          else (st, collect_dep_expr st (name :: outer_vars) value)
      | Expr expr -> (st, collect_dep_expr st outer_vars expr)
    and collect_dep_expr (st : unit Symtable.symbol_table)
        (outer_vars : string list) : expr -> string list = function
      | Id name ->
          if Symtable.mem name st || Symtable.mem name builtins then outer_vars
          else name :: outer_vars
      | If (pred, then_c, else_c) -> (
          let outer_vars = collect_dep_expr st outer_vars pred in
          let outer_vars = collect_dep_expr st outer_vars then_c in
          match else_c with
          | Some else_c -> collect_dep_expr st outer_vars else_c
          | None -> outer_vars )
      | Lambda (args, body) ->
          let st =
            List.fold_left (fun st name -> Symtable.add name () st) st args
          in
          let _, outer_vars = collect_dep_stmts st outer_vars body in
          outer_vars
      | Begin body ->
          let _, outer_vars = collect_dep_stmts st outer_vars body in
          outer_vars
      | Let (bindings, body) ->
          let st =
            List.fold_left
              (fun st binding ->
                let name, _ = binding in
                Symtable.add name () st)
              st bindings
          in
          let outer_vars =
            List.fold_left
              (fun outer_vars binding ->
                let _, expr = binding in
                collect_dep_expr st outer_vars expr)
              outer_vars bindings
          in
          let _, outer_vars = collect_dep_stmts st outer_vars body in
          outer_vars
      | FunCall (func, args) ->
          let outer_vars = collect_dep_expr st outer_vars func in
          List.fold_left (collect_dep_expr st) outer_vars args
      | _ -> outer_vars
    in

    let st = Symtable.from [] in
    let st = List.fold_left (fun st name -> Symtable.add name () st) st args in

    let _, outer_vars = collect_dep_stmts st [] stmts in
    List.sort_uniq String.compare outer_vars
  in

  (* Used to wrap builtin functions in a value_t, so that we can pass
     them as higher order functions *)
  let maybe_wrap_builtin (v : value_t) (builder : L.llbuilder) : value_t =
    let { value = llvalue } = v in
    match L.classify_type (L.element_type (L.type_of llvalue)) with
    | L.TypeKind.Function ->
        let params = L.params llvalue in
        if Array.length params = 0 then v
        else
          let first_param = params.(0) in
          let fp_type = L.type_of first_param in
          if
            L.classify_type fp_type = L.TypeKind.Pointer
            && L.classify_type (L.element_type fp_type) = L.TypeKind.Integer
          then (
            (* Start wrapping *)
            let func_ptr =
              L.build_bitcast llvalue i8_ptr_t "func_ptr" builder
            in
            let alloca = build_malloc value_type "builtin_wraper" builder in
            let arg_num = Array.length params - 1 in
            build_literal alloca type_func value_type_func
              [
                (1, func_ptr);
                (2, L.const_null i8_ptr_t);
                (3, L.const_int i8_t arg_num);
                (4, L.const_int i8_t arg_num);
              ]
              builder;
            { value = alloca; real_func = Some llvalue } )
          else v
    | _ -> v
  in

  let rec build_stmt_block (func : L.llvalue) (decl : value_t)
      (st : symbol_table) (builder : L.llbuilder) (stmts : stmt list) :
      symbol_table * L.llbuilder * value_t =
    List.fold_left
      (fun ctx stmt ->
        let st, builder, decl = ctx in
        build_stmt func decl st builder stmt)
      (st, builder, decl) stmts
  and build_stmt (func : L.llvalue) (decl : value_t) (st : symbol_table)
      (builder : L.llbuilder) : stmt -> symbol_table * L.llbuilder * value_t =
    function
    | Define (name, value) ->
        let value_decl = declare_expr name st builder value in
        let st = Symtable.add name value_decl st in
        let builder, value_decl = build_expr func value_decl st builder value in
        (st, builder, value_decl)
    | Set (name, value) ->
        let decl =
          match Symtable.find name st with
          | Some decl -> decl
          | _ -> raise (Failure "Undefined variable")
        in
        let value_decl = declare_expr name st builder value in
        let builder, value_decl = build_expr func value_decl st builder value in
        let st = Symtable.add name value_decl st in
        build_memcpy (llvalue_of value_decl) (llvalue_of decl) builder;
        (st, builder, decl)
    | Expr expr ->
        let builder, decl = build_temp_expr func "" st builder expr in
        (st, builder, decl)
  and declare_expr (name : string) (st : symbol_table) (builder : L.llbuilder) :
      expr -> value_t = function
    | Id name -> (
        match Symtable.find name st with
        | Some value -> maybe_wrap_builtin value builder
        | None -> raise (Failure "Undefined variable") )
    | Lambda (args, body) ->
        let llvalue = build_malloc value_type name builder in
        let arg_size = List.length args in
        let func_type = function_type arg_size in
        let func = L.define_function "lambda" func_type the_module in
        L.set_linkage L.Linkage.Internal func;
        { value = llvalue; real_func = Some func }
    | _ ->
        let llvalue = build_malloc value_type name builder in
        make_val llvalue
  and build_temp_expr (func : L.llvalue) (name : string) (st : symbol_table)
      (builder : L.llbuilder) (expr : expr) : L.llbuilder * value_t =
    let value = declare_expr name st builder expr in
    build_expr func value st builder expr
  and build_expr (the_func : L.llvalue) (decl : value_t) (st : symbol_table)
      (builder : L.llbuilder) : expr -> L.llbuilder * value_t = function
    | IntLit v ->
        build_literal (llvalue_of decl) type_integer value_type_int
          [ (1, L.const_int i64_t v) ]
          builder;
        (builder, decl)
    | SFloat v ->
        build_literal (llvalue_of decl) type_float value_type_real
          [ (1, L.const_float f64_t v)] builder;
          (builder, decl)
    | BoolLit v ->
        build_literal (llvalue_of decl) type_bool value_type_bool
          [ (1, L.const_int i1_t (if v then 1 else 0)) ]
          builder;
        (builder, decl)
    | CharLit v ->
        build_literal (llvalue_of decl) type_char value_type_char
          [ (1, L.const_int i8_t (Char.code v)) ]
          builder;
        (builder, decl)
    | StrLit str ->
        let str_ptr = L.build_global_stringptr str "string_literal" builder in
        build_literal (llvalue_of decl) type_string value_type_string
          [ (1, str_ptr); (2, L.const_int i64_t (String.length str)) ]
          builder;
        (builder, decl)
    | Symbol name ->
        let str_ptr = L.build_global_stringptr name "symbol_literal" builder in
        build_literal (llvalue_of decl) type_symbol value_type_symbol
          [ (1, str_ptr); (2, L.const_int i64_t (String.length name)) ]
          builder;
        (builder, decl)
    | Begin body ->
        let _, builder, decl = build_stmt_block the_func decl st builder body in
        (builder, decl)
    | Let (bindings, body) ->
        (* This "let" is in fact "letrec" in standard scheme *)
        let st, bindings =
          Utils.fold_map
            (fun st binding ->
              let name, expr = binding in
              let decl = declare_expr name st builder expr in
              (Symtable.add name decl st, (name, decl, expr)))
            st bindings
        in
        let builder, st =
          List.fold_left
            (fun ctx binding ->
              let name, decl, expr = binding in
              let builder, st = ctx in
              let builder, decl = build_expr the_func decl st builder expr in
              let st = Symtable.add name decl st in
              (builder, st))
            (builder, st) bindings
        in
        let _, builder, decl = build_stmt_block the_func decl st builder body in
        (builder, decl)
    | If (pred, then_c, else_c) ->
        let builder, { value = pred_val } =
          build_temp_expr the_func "pred" st builder pred
        in
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
        let then_bb = L.append_block context "then" the_func in
        let then_builder, { value = then_value } =
          build_temp_expr the_func "then" st
            (L.builder_at_end context then_bb)
            then_c
        in
        let new_then_bb = L.insertion_block then_builder in
        let else_bb = L.append_block context "else" the_func in
        let else_builder, else_value =
          match else_c with
          | Some expr ->
              let builder, { value } =
                build_temp_expr the_func "else" st
                  (L.builder_at_end context else_bb)
                  expr
              in
              (builder, value)
          | None -> (builder, L.const_null value_ptr_type)
        in
        let new_else_bb = L.insertion_block else_builder in

        let end_bb = L.append_block context "end" the_func in
        let build_br_end = L.build_br end_bb in
        add_terminal (L.builder_at_end context then_bb) build_br_end;
        add_terminal (L.builder_at_end context else_bb) build_br_end;
        add_terminal then_builder build_br_end;
        add_terminal else_builder build_br_end;
        ignore (L.build_cond_br value then_bb else_bb orig_builder);
        let builder = L.builder_at_end context end_bb in
        let phi =
          L.build_phi
            [ (then_value, new_then_bb); (else_value, new_else_bb) ]
            "ifval" builder
        in
        (builder, make_val phi)
    | FunCall (func, args) -> (
        let builder, func =
          match func with
          | Id name -> (
              match Symtable.find name st with
              | Some func -> (builder, func)
              | None -> raise (Failure ("Function " ^ name ^ " is not defined"))
              )
          | expr -> build_temp_expr the_func "" st builder expr
        in

        (* Reference: https://groups.google.com/forum/#!topic/llvm-dev/_xy_3ZpQFLI *)
        let { value = func; real_func } = func in
        match L.classify_type (L.element_type (L.type_of func)) with
        (* User defined functions *)
        | L.TypeKind.Struct -> (
            let arg_size = List.length args in
            (* check if value is really a function *)
            ( match real_func with
            | Some _ -> ()
            | None ->
                ignore
                  (L.build_call check_func_func
                     [| func; L.const_int i8_t arg_size |]
                     "" builder) );
            (* prepare arguments *)
            let builder, args =
              Utils.fold_map (build_temp_expr func "" st) builder args
            in
            (* bitcast the value to a function for calling *)
            let function_type = function_type arg_size in
            let casted =
              L.build_bitcast func
                (L.pointer_type value_type_func)
                "func_value" builder
            in
            (* access link *)
            let access_link =
              let access_link_ptr =
                L.build_struct_gep casted 2 "access_link_ptr" builder
              in
              let access_link =
                L.build_load access_link_ptr "access_link" builder
              in
              access_link
            in
            let args = List.map llvalue_of args in
            let args = Array.of_list (access_link :: args) in
            match real_func with
            | Some real_func ->
                let ret = L.build_call real_func args "ret" builder in
                (builder, make_val ret)
            | None ->
                (* call the function pointer *)
                let func_ptr_ptr =
                  L.build_struct_gep casted 1 "func_ptr_ptr" builder
                in
                let func_ptr = L.build_load func_ptr_ptr "func_ptr" builder in
                let func =
                  L.build_inttoptr
                    (L.build_ptrtoint func_ptr i64_t "func_ptr_val" builder)
                    (L.pointer_type function_type)
                    "func" builder
                in
                let ret = L.build_call func args "ret" builder in
                (builder, make_val ret) )
        (* Builtin functions *)
        | L.TypeKind.Function ->
            let builder, args =
              Utils.fold_map (build_temp_expr the_func "" st) builder args
            in
            let args = List.map llvalue_of args in
            let args = L.const_null i8_ptr_t :: args in
            let args = Array.of_list args in
            let ret = L.build_call func args "ret" builder in
            (builder, make_val ret)
        | _ -> raise (Failure "Unexpected function type") )
    | Lambda (args, body) ->
        let { real_func = func } = decl in
        let func =
          match func with
          | Some func -> func
          | None ->
              raise
                (Failure
                   "Invariant violation, declare_expr should have set up \
                    real_func")
        in
        let arg_size = List.length args in
        let deps = collect_dependency body args in
        let func_ptr = L.build_bitcast func i8_ptr_t "func_ptr" builder in
        (* set up access link *)
        let access_link_type =
          L.struct_type context (Array.make (List.length deps) value_ptr_type)
        in
        let access_link = build_malloc access_link_type "access_link" builder in

        List.iteri
          (fun index dep ->
            let field =
              L.build_struct_gep access_link index "access_link_field" builder
            in
            ignore
              (L.build_store
                 ( match Symtable.find dep st with
                 | Some { value } -> value
                 | None -> raise (Failure ("Can't find outer variable " ^ dep))
                 )
                 field builder))
          deps;
        (* Add arguments to the symbol table *)
        let new_st =
          List.fold_left2
            (fun st arg arg_name ->
              match arg_name with
              | "" -> st (* ignore the first argument *)
              | _ -> Symtable.add arg_name arg st)
            st
            (List.map make_val (Array.to_list (L.params func)))
            (* The first argument of a function is the
               access link, and it will not be added to
               the symbol table *)
            ("" :: args)
        in
        (* Build the function body *)
        build_func_body deps func new_st body;
        (* Wrap the function in a value_type instance *)
        let access_link_void_ptr =
          L.build_bitcast access_link i8_ptr_t "access_link_void_ptr" builder
        in
        build_literal (llvalue_of decl) type_func value_type_func
          [
            (1, func_ptr);
            (2, access_link_void_ptr);
            (3, L.const_int i8_t arg_size);
            (4, L.const_int i8_t arg_size);
          ]
          builder;
        (builder, decl)
    | Cons (hd, tl) ->
        let builder, { value = car } =
          build_temp_expr the_func "car" st builder hd
        in
        let builder, { value = cdr } =
          build_temp_expr the_func "cdr" st builder tl
        in
        build_literal (llvalue_of decl) type_cons value_type_cons
          [ (1, car); (2, cdr) ] builder;
        (builder, decl)
    | Nil -> (builder, make_val (L.const_null value_ptr_type))
    | Id _ -> (builder, decl)
  and build_func_body (deps : string list) (func : L.llvalue)
      (st : symbol_table) (stmts : stmt list) : unit =
    let builder = L.builder_at_end context (L.entry_block func) in
    (* The first argument is always access link *)
    let access_link_void_ptr = (L.params func).(0) in
    let access_link_type =
      L.struct_type context (Array.make (List.length deps) value_ptr_type)
    in
    let access_link =
      L.build_bitcast access_link_void_ptr
        (L.pointer_type access_link_type)
        "access_link" builder
    in
    let st =
      (* Reimport referenced variables into the symbol table from the
         access link *)
      List.fold_left
        (fun st var ->
          let index, name = var in
          let field_ptr =
            L.build_struct_gep access_link index "outer_var_ptr" builder
          in
          let field = L.build_load field_ptr "outer_var" builder in
          let outer_value =
            match Symtable.find name st with
            | Some outer_value -> outer_value
            | None ->
                raise
                  (Failure
                     "Invariant violation, symbol table should container outer \
                      variables")
          in
          Symtable.add name
            (copy_value_metadata outer_value (make_val field))
            st)
        st
        (List.mapi (fun i name -> (i, name)) deps)
    in
    let value = make_val (build_malloc value_type "" builder) in
    let _, builder, ret_value = build_stmt_block func value st builder stmts in
    ignore (L.build_ret (llvalue_of ret_value) builder)
  and build_main_func_body (func : L.llvalue) (st : symbol_table)
      (stmts : stmt list) =
    let builder = L.builder_at_end context (L.entry_block func) in
    let value = make_val (build_malloc value_type "" builder) in
    let _, builder, _ = build_stmt_block func value st builder stmts in
    ignore (L.build_ret (L.const_int i32_t 0) builder)
  in

  let main_func =
    let main_func_type = L.function_type i32_t [||] in
    L.define_function "main" main_func_type the_module
  in
  build_main_func_body main_func builtins stmts;
  the_module
