module S = Streamz

type point = {x:float; y:float;}

let pi_known = 3.141592653589793238462643383279502884197169399375105820974944592307816406286

let gen_point xr yr =
  {x=xr; y=yr}

let origin =  (gen_point 0.0 0.0)

let distance_between_points p q =
  (p.x -. q.x) *. (p.x -. q.x) +. (p.y -. q.y) *. (p.y -. q.y)

let distance_from_origin p =
  distance_between_points p origin

let points =
  let rec fibgen point =
    S.Cons(point, fun () -> fibgen (gen_point (Random.float 1.0) (Random.float 1.0))) in
  fibgen (gen_point (Random.float 1.0) (Random.float 1.0))

let within point =
  let d = distance_from_origin point in
  match d with
    | d when d < 1.0 -> true
    | _ -> false

let count_within l =
  List.length (List.filter (fun point -> within point) (List.tl l))

let find_points_within_radius ~number_of_samples:s =
  let rec aux s acc =
    match s, acc with
      | 0, acc -> acc
      | s, acc -> aux (s-1000) (acc + count_within (S.take points 1001)) in
  aux s 0
