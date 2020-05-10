open Ast
open Sys
open Printf

(* Switch between print Ast, Ast after macro expansion, Sast.
Otherwise execute source code as default. *)
type action = Ast | Mast | Sast | IR | Exec

let _ =
  (* Accept file name and support optional switch *)
  let action = ref Exec in
  let set_action a () = action := a in
  let speclist =
    [
      ("-a", Arg.Unit (set_action Ast), "Print the AST");
      ("-m", Arg.Unit (set_action Mast), "Print the AST after macro expansion");
      ("-s", Arg.Unit (set_action Sast), "Print the SAST");
      ("-l", Arg.Unit (set_action IR), "Print the LLVM IR");
      ("-e", Arg.Unit (set_action Exec), "Create executable from source code");
    ]
  in
  let usage_msg = "usage: ./tilisp.native [-a|-m|-s|-l] [file.tlsp]" in
  let channel = ref stdin in
  Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;

  (* Run program *)
  let program = really_input_string !channel (in_channel_length !channel) in
  let stdlib_channel = open_in "stdlib.tisp" in
  let stdlib =
    really_input_string stdlib_channel (in_channel_length stdlib_channel)
  in
  let lexbuf =
    Lexing.from_string
      (match !action with Exec -> stdlib ^ program | _ -> program)
  in
  let program = Parser.program Scanner.token lexbuf in
  let expanded = Macro.expand_all program in
  match !action with
  | Ast ->
      print_endline
        (String.concat "\n\n"
           (List.filter
              (fun x -> String.length x > 0)
              (List.map Ast.string_of_ast program)))
  | Mast ->
      print_endline
        (String.concat "\n\n"
           (List.filter
              (fun x -> String.length x > 0)
              (List.map Ast.string_of_ast expanded)))
  | _ -> (
      let program = Semant.check expanded in
      match !action with
      | Ast -> ()
      | Mast -> ()
      | Sast ->
          print_endline
            (String.concat "\n\n"
               (List.filter
                  (fun x -> String.length x > 0)
                  (Sast.string_of_stmt_block program)))
      | IR -> print_endline (Llvm.string_of_llmodule (Irgen.translate program))
      | Exec ->
          let out = open_out "llvm.out" in
          fprintf out "%s\n" (Llvm.string_of_llmodule (Irgen.translate program));
          close_out out;
          if command "opt -S -O3 llvm.out -o llvm.optimized.out" != 0 then
            raise (Failure "opt: non-zero exit code")
          (* Replace malloc with GC_malloc *)
          else if command "sed -i 's/malloc(i32/GC_malloc(i64/g' llvm.optimized.out" != 0 then
            raise (Failure "sed: non-zero exit code")
          else if command "llc -relocation-model=pic llvm.optimized.out" != 0 then
            raise (Failure "llc: non-zero exit code")
          else if (if file_exists "./bdwgc/libgc.la" then
                     command "./bdwgc/libtool --quiet --mode=link g++ llvm.optimized.out.s -L./ -ltilisp -o a.out ./bdwgc/libgc.la"
                   else
                     command "g++ llvm.optimized.out.s -L./ -ltilisp -L./bdwgc -lgc -o a.out") != 0 then
            raise (Failure "g++: non-zero exit code")
          else () )
