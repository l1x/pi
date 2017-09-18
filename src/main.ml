module P = Pi

let run_experiments ~size_of_experiment:s ~number_of_buckets:n =
  List.map (fun x -> P.count_within x) (P.gen_list s n)

let calculate_pi_with_monte_carlo ~size_of_experiment:s ~number_of_buckets:n =
  let results = run_experiments s n in
  let points_within = List.fold_left (fun x y -> x + (snd y)) 0 results in
  let points_all = List.fold_left (fun x y -> x + (fst y)) 0 results in
  (points_within, points_all)

let main () =
  Random.self_init();
  let num_iter = Sys.argv.(1) in
  let n = max (int_of_string num_iter) 20 in
  print_endline ("Number of iterations :: " ^ (string_of_int n));
  let res = calculate_pi_with_monte_carlo ~size_of_experiment:n ~number_of_buckets:20 in
  let pi_estimated = ((float_of_int (fst res)) /. (float_of_int (snd res))) *. 4.0 in
  print_endline (Printf.sprintf "Pi estimated :: %0.10f Pi original %0.10f Difference :: %0.10f " pi_estimated P.pi_known (P.pi_known -. pi_estimated))

let () =
  main ()
