module P = Pi

let print_results pi_estimated pi_known =
  Printf.sprintf "Pi estimated :: %0.10f Pi known %0.10f Difference :: %0.10f " pi_estimated pi_known (pi_known -. pi_estimated)

let main () =
  let num_iter = Sys.argv.(1) in
  let n = max (int_of_string num_iter) 1000 in
  print_endline ("Number of iterations :: " ^ (string_of_int n));
  let pi_estimated = P.find_pi n in
  print_endline (print_results pi_estimated P.pi_known)

let () =
  main ()
