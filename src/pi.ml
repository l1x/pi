module R = Random

let pi_known = 3.141592653589793238462643383279502884197169399375105820974944592307816406286

type point = {x:float; y:float;}

let origin =	{x=0.0; y=0.0}

let distance_between_points p q =  (p.x -. q.x) *. (p.x -. q.x) +. (p.y -. q.y) *. (p.y -. q.y)

let distance_from_origin p = distance_between_points p origin

let find_pi ~number_of_samples:s =
	Random.self_init();
	let within = ref 0 in
	for i = 1 to s do
		let xr = R.float 1.0 in
		let yr = R.float 1.0 in
		let p = {x=xr; y=yr} in
		if distance_from_origin p < 1.0 then
			within := !within + 1
	done;
	((float_of_int !within) /. (float_of_int s)) *. 4.0
