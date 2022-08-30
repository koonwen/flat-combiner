type 'a t =
  { queue : 'a Queue.t
  ; mutex : Mutex.t
  ; max_size : int
  }

let init ?(size_exponent = 31) () =
  let max_size = 1 lsl size_exponent in
  { queue = Queue.create (); mutex = Mutex.create (); max_size }
;;

let with_mutex mtx f =
  Mutex.lock mtx;
  let v = f () in
  Mutex.unlock mtx;
  v
;;

let rec enqueue t item =
  let ({ queue; mutex; max_size } : 'a t) = t in
  let enqueued =
    with_mutex mutex (fun () ->
      if Queue.length queue < max_size
      then (
        Queue.push item queue;
        true)
      else false)
  in
  if enqueued
  then ()
  else (
    Domain.cpu_relax ();
    enqueue t item)
;;

let dequeue { queue; mutex; _ } = with_mutex mutex (fun () -> Queue.take_opt queue)
let clear { queue; mutex; _ } = with_mutex mutex (fun () -> Queue.clear queue)
let _q = init ()

let rec enqueuer lo hi =
  if lo <= hi
  then (
    let id = Domain.self () in
    enqueue _q (id, lo);
    enqueuer (lo + 1) hi)
  else ()
;;

let dequeuer n =
  let acc = ref [] in
  let rec aux n =
    if n > 0
    then (
      (match dequeue _q with
       | Some v -> acc := v :: !acc
       | None -> ());
      aux (n - 1))
    else ()
  in
  aux n;
  !acc
;;

let clearer () = clear _q