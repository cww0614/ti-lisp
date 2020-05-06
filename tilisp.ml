open Ast

(* Switch between print Ast, Ast after macro expansion, Sast.
Otherwise execute source code as default. *)
type action = Ast | Mast | Sast | IR | Exec

let _ =
  (* Accept file name and support optional switch *)
  let action = ref Exec in
  let set_action a () = action := a in
  let speclist = [
    ("-a", Arg.Unit (set_action Ast), "Print the AST");
    ("-m", Arg.Unit (set_action Mast), "Print the AST after macro expansion");
    ("-s", Arg.Unit (set_action Sast), "Print the SAST");
    ("-l", Arg.Unit (set_action IR), "Print the LLVM IR");
  ] in
  let usage_msg = "usage: ./tilisp.native [-a|-m|-s|-l] [file.tlsp]" in
  let channel = ref stdin in
  Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;

  (* Run program *)
  let lexbuf = Lexing.from_channel !channel in
  let program = Parser.program Scanner.token lexbuf in
  let expanded = Macro.expand_all program in
  match !action with
    Ast -> print_endline (String.concat "\n\n"
                            (List.filter
                              (fun x -> String.length x > 0)
                              (List.map Ast.string_of_ast program)))
   | Mast -> print_endline (String.concat "\n\n"
                              (List.filter
                                (fun x -> String.length x > 0)
                                (List.map Ast.string_of_ast expanded)))
  | _ -> let program = Semant.check expanded in
    match !action with
      Ast -> ()
    | Mast -> ()
    | Sast -> print_endline (String.concat "\n\n"
                              (List.filter
                                (fun x -> String.length x > 0)
                                (Sast.string_of_stmt_block program)))
    | IR -> print_endline (Llvm.string_of_llmodule (Irgen.translate ()))

    (* TODO: Replace with llvm.out after done with irgen *)
    | Exec -> print_endline (String.concat "\n\n"
                              (List.filter
                                (fun x -> String.length x > 0)
                                (Sast.string_of_stmt_block program)))
