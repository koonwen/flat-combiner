(* module type DataStructure = sig
  type t
  type opcode

  val to_string : t -> string
end

module Make (DS : DataStructure) : sig
  type t
  type elt = DS.t
  type opcode = DS.opcode
  (* This should expose all the functions in the opcode *)

  val _publish : DS.opcode -> 'b
  val _scan_combine_apply : unit -> unit
end *)