(* Building this with "ocamlbuild -lib unix testing.native" *)
module S = String;;

(* List of tuple (testcase description, testname, arguments for tilisp) *)
let ls_testcases = [
  ("[scanner] comments & identifier", "scanner_comments_id", "-a");

  ("[parser] unpaired bracket", "parser_unpaired_bracket", "-a");

  ("[parser] incomplete string", "parser_invalid_string", "-a");

  ("[parser] parsing quote", "parser_quote", "-a");

  ("[parser] parsing char literal", "parser_char", "-a");

  ("[semant] undefined variable", "semant_undefined_variable", "-s");

  ("[parser] simple list", "parser_simple_list", "-a");

  ("[macro] parse expansion in macro", "parser_macro_expansion", "-a");

  ("[macro] macro definition", "macro_set_nil", "-s");

  ("[macro] multiple rules", "macro_incre", "-s");

  ("[macro] with reserved keywords", "macro_reserved_keywords", "-s");

  ("[semant], built-in functions", "semant_builtin", "-s");

  ("[semant], self-quoted literal", "semant_self_quoted", "-s");

  (* Test SAST for fib. *)
  ("[more] (fib.tisp, SAST)" , "fib", "-s");
  
  (* Test for nested inner defines *)
  ("[semant] inner_define_valid.tisp, SAST", "inner_define_valid", "-s");

  (* Test for nested inner defines *)
  ("[semant] inner_define_invalid.tisp, SAST", "inner_define_invalid", "-s");

  (* Duplicate names in a let bindings:
  unlike Scheme, an error would be thrown out. *)
  ("[semant] duplicate names in one let binding", "let_duplications", "-s");

  (* Multi-level nested 'define' *)
  ("[semant] Multi-level nested 'define' ", "inner_define_multilevel", "-s");

  (* Duplcated parameters in lambda expression *)
  ("[semant] Duplcated parameters in lambda expression", "lambda_duplications", "-s");

  (* ("[irg] built-in function call.", "irg_builtin_func", "-l"); *)

  ("[exe] built-in display", "exe_display", "-e");
  ("[exe] built-in display string", "exe_display_string", "-e");

  ("[exe] define variable", "exe_basic1", "-e");

  ("[exe] set global variables", "exe_basic2", "-e");

  ("[exe] op plus", "exe_plus", "-e");
  ("[exe] op variadic plus", "exe_variadic_plus", "-e");
  ("[exe] op variadic mult", "exe_variadic_mult", "-e");
  ("[exe op string concat]", "exe_string_concat", "-e")

] in

let read_lines ic : string list =
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s when s = "" -> loop acc
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop [] in
let string_to_list s  = 
  let ls = ref [] in
  let _ = S.iter (fun c -> ignore(ls := c::!ls)) s in
  List.rev (!ls) in

let subset_string (full_str: string) (pattern: string) : bool =
  let s = string_to_list full_str and p = string_to_list pattern in
  let rec match_char_list (s: char list) (p: char list) : bool =
  match (s, p) with
   (_, []) -> true
  | (h1::t1, h2::t2) when h1 = h2 -> match_char_list t1 t2
  | (_::t1, ls2) -> match_char_list t1 ls2
  | _ -> false in
  match_char_list s p in

let rec compare_list (a: string list) (b: string list) : bool =
  match (a, b) with
    ([], []) -> true
  | (h1::t1, ls2) when (S.trim h1) = "" -> compare_list t1 ls2
  | (ls1, h2::t2) when (S.trim h2) = "" -> compare_list ls1 t2
  | (h1::t1, h2::t2) when subset_string (S.trim h1) (S.trim h2) 
      -> compare_list t1 t2
  | _ -> false in

let single_test (testcase : (string * string * string)) = 
  let (title, test_name, test_arg) = testcase in
  let cmd_run = "./tilisp.native "^test_arg^" tests/"^test_name^".tisp" in
  let file_expected = "tests/"^test_name^".log" in
  let (output, inp, errors) =  match test_arg with
    "-e" -> begin
      if Sys.command cmd_run != 0 then
      raise (Failure "executable building fails.")
      else Unix.open_process_full "./a.out" [| |]
    end
  | _ -> Unix.open_process_full cmd_run [| |] in
  let ls_stdout = read_lines output in
  let ls_errors = read_lines errors in
  let ls_actual = ls_stdout @ ls_errors in
  let ic_expected = open_in file_expected in 
  let ls_expected = read_lines ic_expected in
  let _ = ignore(Unix.close_process_full (output, inp, errors));
  if (List.length ls_actual) = 0 then
    raise (Failure ("Empty output file: "^test_name)) in
  if (List.length ls_expected) = 0 then begin
    print_endline ("Test case: "^title^" [unknown expected output]");
    print_string "Actual:\n"; List.iter print_endline ls_actual
  end else if (compare_list ls_actual ls_expected) then
    print_endline ("Test case: "^title^" [passed].")
  else begin
      print_endline ("Test case: "^title^" [failed].");
      print_string "Expected:\n"; List.iter print_endline ls_expected;
      print_string "Actual:\n"; List.iter print_endline ls_actual
  end in

List.iter single_test ls_testcases; 
"Governor Cuomo: we need more testing!"
