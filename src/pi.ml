type point = {x:float; y:float;}

let point_to_string { x = x; y = y } =
	Printf.sprintf "%.2f %.2f" x y

let gen_point xr yr =
	{x=xr; y=yr}

let distance_between_points p q =
	(p.x -. q.x) *. (p.x -. q.x) +. (p.y -. q.y) *. (p.y -. q.y)

let distance_from_origin c =
	distance_between_points c (gen_point 0.0 0.0)

let count_within ~counter:n =
	let rec count_within_aux ~counter:n ~within:m =
		match n, m with
			| 0, m -> m
			| n, m ->
				let cc = gen_point (Random.float 1.0) (Random.float 1.0) in
				let dist = distance_from_origin cc in
				match dist with
					| dist when dist <= 1.0 -> count_within_aux ~counter:(n - 1) ~within:(m + 1)
					| dist when dist > 1.0 -> count_within_aux ~counter:(n - 1) ~within:m
					| _ -> 0 in
	count_within_aux ~counter:n ~within:0

let count_within_stepping ~counter:n ~stepping:s =
	let rec count_within_stepping_aux ~counter:n ~within:m ~acc:acc =
		match n, m, acc with
			| n, m, acc when n <= 0 -> m
			| n, m, acc ->
					let c = count_within s in
					let pi = ((float_of_int m) /. (float_of_int acc)) *. 4.0 in
					print_endline ("Pi currently :: " ^ (Printf.sprintf "%.5f" pi));
					count_within_stepping_aux ~counter:(n-s) ~within:(m+c) ~acc:(acc+s) in
	count_within_stepping_aux ~counter:n ~within:0 ~acc:0

let main =
	Random.self_init();
	let num_iter = Sys.argv.(1) in
	let n = int_of_string num_iter in
	print_endline ("Number of iterations :: " ^ num_iter);
	let pi_estimated = ((float_of_int (count_within_stepping n (n / 20))) /. (float_of_int n)) *. 4.0 in
	let pi_known = 3.141592653589793238462643383279502884197169399375105820974944592307816406286 in
	let ratio = (pi_known -. pi_estimated) /. pi_known in
	print_endline ("Pi's value :: " ^ (string_of_float pi_estimated) ^ "	Error rate ::" ^ (Printf.sprintf "%.5f" ratio))

let () =
	main
