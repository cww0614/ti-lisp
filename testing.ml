let ls_testcases = [
  (* Test SAST for fib.tlsp. Set the name of your test, inputs, switches,
  expected output, and generated code. Compares if test_output and test_gen
  are the same. *)
  ("Test case: (fib.tslp, SAST)" , "fib", "-s");
  
  (* Test for nested inner defines *)
  ("Test case: (inner_define_valid.tslp, SAST)", "inner_define_valid", "-s");

  (* Test for nested inner defines *)
  ("Test case: (inner_define_invalid.tslp, SAST)", "inner_define_invalid", "-s")
] in

let read_lines ic : string list =
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop [] in
let compare_list a b =
  let compare_elem a b = (a=b) in
  List.for_all2 compare_elem a b in
let single_test (testcase : (string * string * string)) = 
  let (title, test_name, test_arg) = testcase in
  let cmd_run = "./tilisp.native "^test_arg^" tests/"^test_name^".tlsp" in
  let file_expected = "tests/"^test_name^".output" in
  let (output, inp, errors) = Unix.open_process_full cmd_run [| |] in
  let ls_stdout = read_lines output in
  let ls_errors = read_lines errors in
  let ls_actual = ls_stdout @ ls_errors in
  let ic_expected = open_in file_expected in 
  let ls_expected = read_lines ic_expected in
  let _ = Unix.close_process_full (output, inp, errors) in
  if (compare_list ls_actual ls_expected) then
  print_endline (title^" passed.") else (print_endline (title^" failed.");
  print_string "Expeceted:\n"; List.iter print_endline ls_expected;
  print_string "Actual:\n"; List.iter print_endline ls_actual) in

List.iter single_test ls_testcases; 
"Governor Cuomo: we need more testing!"
