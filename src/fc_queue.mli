module type Content = sig
  type t

  val to_string : t -> string
end

module FC_queue (T : Content) : sig
  type t
  type elt = T.t

  val create : unit -> t
  val enqueue : t -> elt -> unit
  val dequeue : t -> elt
  val traverse_publist : t -> unit
  val print_queue : t -> unit
end