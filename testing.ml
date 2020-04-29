let _ =

  (* Test SAST for fib.tlsp. Set the name of your test, inputs, switches,
  expected output, and generated code. Compares if test_output and test_gen
  are the same. *)
  let test_name = "Test case 1: (fib.tslp, SAST)" in
  let test_input = "fib.tlsp" in
  let test_arg = "-s" in
  let test_output = "fib.output" in
  let test_gen = "fib.gen" in
  let cmd_gen = "./tilisp.native "^test_arg^" tests/"^test_input^" &> tests/"^test_gen in
  let cmd_compare = "cmp --silent tests/"^test_output^" tests/"^test_gen^
                    " && echo '"^test_name^" passed' || echo '"^test_name^" failed'"
  in
  let _ = Sys.command cmd_gen in
  let _ = Sys.command cmd_compare 
  in

  "More tests..."