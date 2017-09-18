type 'a stream = Nil | Cons of 'a * (unit -> 'a stream)
val hd : 'a stream -> 'a
val tl : 'a stream -> 'a stream
val nth : 'a stream -> int -> 'a
val from_list : 'a list -> 'a stream
val take : 'a stream -> int -> 'a list
