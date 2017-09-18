type point = {x:float; y:float;}

let pi_known = 3.141592653589793238462643383279502884197169399375105820974944592307816406286

let percentage_error pi_estimated =
  ((pi_known -. pi_estimated) /. pi_known) *. 100.0

let point_to_string p =
  Printf.sprintf "%.2f %.2f" p.x p.y

let gen_point xr yr =
  {x=xr; y=yr}

let distance_between_points p q =
  (p.x -. q.x) *. (p.x -. q.x) +. (p.y -. q.y) *. (p.y -. q.y)

let distance_from_origin p =
  distance_between_points p (gen_point 0.0 0.0)

let frnd f = floor (f +. 0.5)

(*Split n into roughly m pieces*)
let gen_list n m =
 let f = int_of_float (frnd ((float_of_int n) /. (float_of_int m))) in
 let rec aux n f acc =
   if n > f then
     aux (n - f) f (f::acc)
   else (List.rev (n::acc))
 in
 aux n f []

let count_within ~counter:n =
  let rec count_within_aux ~counter:n ~within:m =
    match n, m with
      | 0, m -> m
      | n, m ->
        let cc = gen_point (Random.float 1.0) (Random.float 1.0) in
        let dist = distance_from_origin cc in
          match dist with
            | dist when dist < 1.0 -> count_within_aux ~counter:(n - 1) ~within:(m + 1)
            | dist when dist >= 1.0 -> count_within_aux ~counter:(n - 1) ~within:m
            | _ -> 0 in
  (n, count_within_aux ~counter:n ~within:0)
