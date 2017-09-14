let main () =
  Random.self_init();
  let num_iter = Sys.argv.(1) in
  let n = int_of_string num_iter in
  print_endline ("Number of iterations :: " ^ num_iter);
  let pi_estimated = ((float_of_int (Pi.count_within_stepping n (n / 20))) /. (float_of_int n)) *. 4.0 in
  let r = Pi.percentage_error pi_estimated in
  print_endline (Pi.pi_and_error pi_estimated (Pi.pi_known -. pi_estimated) r)

let () =
  main ()
