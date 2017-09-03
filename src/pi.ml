let range ~small:a ~big:b =
  let rec range_aux ~small:a ~big:b ~accumulator:acc =
    if a > b then acc
    else range_aux ~small:(a+1) ~big:b ~accumulator:(a :: acc) in
  List.rev (range_aux ~small:a ~big:b ~accumulator:[])

let take ~list:l ~count:n =
  if List.length l <= n
    then l
  else
    let rec take_aux ~list:l ~count:n ~acc:acc =
      let len = List.length acc in
      match l, len with
        | _, len when len = n  -> List.rev acc
        | [], _ -> []
        | h :: t, _ -> take_aux ~list:t ~count:n ~acc:(h :: acc) in
    take_aux ~list:l ~count:n ~acc:[]

let drop ~list:l ~count:n =
  if List.length l < n
    then []
  else
    let rec drop_aux ~list:l ~count:n =
      match l, n with
        | l, 0 -> l
        | h :: t, n -> drop_aux ~list:t ~count:(n-1) in
    drop_aux ~list:l ~count:n

(*
This function does not support overlapping partitions
https://github.com/clojure/clojure/blob/clojure-1.9.0-alpha14/src/clj/clojure/core.clj#L3157
*)
let partition ~list:l ~partition_size:s =
  let rec partition_aux ~list:l ~partition_size:s ~acc:acc =
    match l, acc with
      | [], acc -> List.rev acc
      | l, acc -> partition_aux ~list:(drop ~list:l ~count:s) ~partition_size:s ~acc:((take ~list:l ~count:s) :: acc) in
  partition_aux ~list:l ~partition_size:s ~acc:[]

type coord = {x:float; y:float;}

let coord_to_string { x = x; y = y } =
  Printf.sprintf "%.2f %.2f" x y;;

let gen_coord xr yr =
  {x=xr; y=yr}

let distance c =
  c.x *. c.x +. c.y *. c.y

let count_within ~counter:n =
  let rec count_within_aux ~counter:n ~within:m =
    match n, m with
      | 0, m -> m
      | n, m ->
        let cc = gen_coord (Random.float 1.0) (Random.float 1.0) in
        let dist = distance cc in
        match dist with
          | dist when dist <= 1.0 -> count_within_aux ~counter:(n - 1) ~within:(m + 1)
          | dist when dist > 1.0 -> count_within_aux ~counter:(n - 1) ~within:m
          | _ -> 0 in
  count_within_aux ~counter:n ~within:0

let main =
  Random.self_init();
  let num_iter = Sys.argv.(1) in
  let n = int_of_string num_iter in
  print_endline ("Number of iterations :: " ^ num_iter);
  let pi = ((float_of_int (count_within n)) /. (float_of_int n)) *. 4.0 in
  let real_pi = 3.14159 in
  let ratio = (real_pi -. pi) /. real_pi in
  print_endline ("Pi's value :: " ^ (string_of_float pi) ^ "  Error rate ::" ^ (Printf.sprintf "%.5f" ratio))

let () =
  main
