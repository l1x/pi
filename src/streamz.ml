(* http://www.cs.cornell.edu/courses/cs3110/2011fa/supplemental/lec24-streams/streams.htm *)
type 'a stream = Nil | Cons of 'a * (unit -> 'a stream)

(* head of a stream *)
let hd (s : 'a stream) : 'a =
  match s with
    Nil -> failwith "hd"
  | Cons (x, _) -> x

(* tail of a stream *)
let tl (s : 'a stream) : 'a stream =
  match s with
    Nil -> failwith "tl"
  | Cons (_, g) -> g () (* get the tail by evaluating the thunk *)

(* n-th element of a stream *)
let rec nth (s : 'a stream) (n : int) : 'a =
  if n = 0 then hd s else nth (tl s) (n - 1)

(* make a stream from a list *)
let from_list (l : 'a list) : 'a stream =
  List.fold_right (fun x s -> Cons (x, fun () -> s)) l Nil

(* make a list from the first n elements of a stream *)
let rec take (s : 'a stream) (n : int) : 'a list =
  if n <= 0 then [] else
  match s with
    Nil -> []
  | _ -> hd s :: take (tl s) (n - 1)

(* Stream end*)
