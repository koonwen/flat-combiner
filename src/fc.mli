type elt
type op_thunk
type op_res

module PubRecord : sig
  type t =
    { mutable op_thunk : op_thunk
    ; mutable result : op_res
    ; mutable age : int
    ; mutable active : bool
    ; id : int
    }
end

type pub_list = PubRecord.t list

type t =
  { queue : elt Queue.t
  ; lock : Mutex.t
  ; pub_list : pub_list Atomic.t
  ; mutable count : int
  }

val scan_combine_apply : t -> PubRecord.t -> op_res
