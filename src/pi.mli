(*
 * Point in a two-dimensional Euclidean space
 *)
type point
val pi_known : float
val origin : point
(*
 * 'Euclidean distance or Euclidean metric is the "ordinary" straight-line distance between
 * two points in Euclidean space. With this distance, Euclidean space becomes a metric space.
 * The associated norm is called the Euclidean norm.
 * Older literature refers to the metric as Pythagorean metric.'
 * https://en.wikipedia.org/wiki/Euclidean_distance
 *)
val distance_between_points : point -> point -> float
val distance_from_origin : point -> float
val find_pi : number_of_samples:int -> float
