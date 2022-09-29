module Util = Util

let id_gen max =
  let internal_state = ref 0 in
  let aux () =
    let id = !internal_state in
    incr internal_state;
    if !internal_state >= max then internal_state := 0;
    id
  in
  aux
;;

module Make (T : sig
  val num_threads : int
end) =
struct
  let num_threads = T.num_threads
  let id_gen = id_gen num_threads
  let _q = Queue.create ()
  let _fcq = Fc_generic.create _q
  let get_domain_id () = (Domain.self () :> int)

  let enq ?(id = id_gen ()) v =
    match
      Fc_generic.apply id _fcq (fun () ->
        Queue.push v _q;
        None)
    with
    | None -> ()
    | _ -> failwith "enq Impossible"
  ;;

  let deq ?(id = id_gen ()) () = Fc_generic.apply id _fcq (fun () -> Queue.take_opt _q)

  let clear ?(id = id_gen ()) () =
    match
      Fc_generic.apply id _fcq (fun () ->
        Queue.clear _q;
        None)
    with
    | None -> ()
    | _ -> failwith "clear Impossible"
  ;;

  let rec enqueuer ?(id = id_gen ()) lo hi =
    if lo <= hi
    then (
      enq ~id (id, lo);
      enqueuer ~id (lo + 1) hi)
    else ()
  ;;
end
