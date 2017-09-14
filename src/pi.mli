(*
 * Point in a two-dimensional Euclidean space
 *)
type point = {x:float; y:float;}
val pi_known : float
val point_to_string : point -> string
val gen_point : float -> float -> point
(*
 * 'Euclidean distance or Euclidean metric is the "ordinary" straight-line distance between
 * two points in Euclidean space. With this distance, Euclidean space becomes a metric space.
 * The associated norm is called the Euclidean norm.
 * Older literature refers to the metric as Pythagorean metric.'
 * https://en.wikipedia.org/wiki/Euclidean_distance
 *)
val distance_between_points : point -> point -> float
val distance_from_origin : point -> float
val count_within : counter:int -> int
val count_within_stepping : counter:int -> stepping:int -> int
val percentage_error : float -> float
val pi_and_error : float -> float -> float -> string
