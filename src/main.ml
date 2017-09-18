module P = Pi

let main () =
  Random.self_init();
  let num_iter = Sys.argv.(1) in
  let n = max (int_of_string num_iter) 1000 in
  print_endline ("Number of iterations :: " ^ (string_of_int n));
  let within = P.find_points_within_radius n in
  let pi_estimated = ((float_of_int within) /. (float_of_int n)) *. 4.0 in
  print_endline (Printf.sprintf "Pi estimated :: %0.10f Pi original %0.10f Difference :: %0.10f " pi_estimated P.pi_known (P.pi_known -. pi_estimated))

let () =
  main ()
