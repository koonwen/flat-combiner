let with_mutex mtx f =
  Mutex.lock mtx;
  let v = f () in
  Mutex.unlock mtx;
  v
;;

(* this should be a variant! *)
type 'a t =
  { returned : 'a option ref
  ; awaiting : ('a -> unit) List.t option ref
  ; mutex : Mutex.t
  ; lock_holding : bool
  }

let empty () : 'a t =
  { returned = ref None
  ; awaiting = ref (Some [])
  ; mutex = Mutex.create ()
  ; lock_holding = false
  }
;;

let await promise f =
  let ({ awaiting; mutex; returned; _ } : 'a t) = promise in
  with_mutex mutex (fun () ->
    match !awaiting with
    | Some awaiting_val ->
      awaiting := Some (f :: awaiting_val);
      `Task
    | None ->
      (match !returned with
       | None -> assert false
       | Some v -> `Already_done v))
;;

let fill { returned; awaiting; mutex; _ } value =
  with_mutex mutex (fun () ->
    assert (Option.is_none !returned);
    returned := Some value;
    let maybe_awaiting_val = !awaiting in
    awaiting := None;
    match maybe_awaiting_val with
    | None -> assert false
    | Some awaiting_val -> awaiting_val)
;;
