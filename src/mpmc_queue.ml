type 'a t =
  { array : 'a Option.t Atomic.t Array.t
  ; head : int Atomic.t
  ; tail : int Atomic.t
  ; mask : int
  }

let init ?(size_exponent = 15) () : 'a t =
  let size = 1 lsl size_exponent in
  let array = Array.init size (fun _ -> Atomic.make None) in
  let mask = size - 1 in
  let head = Atomic.make 0 in
  let tail = Atomic.make 0 in
  { array; head; tail; mask }
;;

let enqueue { array; tail; mask; _ } element =
  let index = Atomic.fetch_and_add tail 1 land mask in
  let cell = Array.get array index in
  while not (Atomic.compare_and_set cell None (Some element)) do
    while Option.is_some (Atomic.get cell) do
      ()
    done
  done
;;

let dequeue queue =
  let ({ array; head; tail; mask; _ } : 'a t) = queue in
  let head_value = Atomic.get head in
  let tail_value = Atomic.get tail in
  if head_value >= tail_value
  then None
  else (
    let old_head = Atomic.fetch_and_add head 1 in
    let cell = Array.get array (old_head land mask) in
    let rec take_or_rollback () =
      let value = Atomic.get cell in
      if Option.is_some value && Atomic.compare_and_set cell value None
      then (
        assert (Option.is_some value);
        value)
      else if Atomic.get tail <= old_head
              && Atomic.compare_and_set head (old_head + 1) old_head
      then None
      else take_or_rollback ()
    in
    take_or_rollback ())
;;

let log ~thr s =
  let s =
    match s with
    | None -> "Empty?"
    | Some s -> s
  in
  Printf.printf "%s: %s\n" thr s;
  Stdlib.flush Stdlib.stdout
;;

let _test_1 () =
  let queue = init ~size_exponent:1 () in
  let a =
    let log = log ~thr:"A" in
    Domain.spawn (fun () ->
      enqueue queue "a";
      enqueue queue "b";
      enqueue queue "c";
      dequeue queue |> log;
      log (Some "done a"))
  in
  let b =
    let log = log ~thr:"B" in
    Domain.spawn (fun () ->
      dequeue queue |> log;
      enqueue queue "d";
      dequeue queue |> log;
      dequeue queue |> log;
      log (Some "done b"))
  in
  Domain.join b |> ignore;
  Domain.join a |> ignore;
  ()
;;

let _q = ref (init ~size_exponent:24 ())

let recreate () =
  let new_q = init ~size_exponent:24 () in
  _q := new_q
;;

let rec enqueuer lo hi =
  if lo <= hi
  then (
    let id = Domain.self () in
    enqueue !_q (id, lo);
    enqueuer (lo + 1) hi)
  else ()
;;

let dequeuer n =
  let acc = ref [] in
  let rec aux n =
    if n > 0
    then (
      (match dequeue !_q with
       | Some v -> acc := v :: !acc
       | None -> ());
      aux (n - 1))
    else ()
  in
  aux n;
  !acc
;;