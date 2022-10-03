module T = Domainslib.Task

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

let () =
  let n = 100_000 in
  let q = init () in
  let pool = T.setup_pool ~num_additional_domains:7 () in
  T.run pool (fun _ ->
    let threads = Array.make 24 (T.async pool (fun () -> ())) in
    for i = 0 to 23 do
      threads.(i)
        <- T.async pool (fun () ->
             for i = 0 to n do
               enqueue q i
             done)
    done;
    Array.iter (T.await pool) threads)
;;
(* let q = Queue.create () in
  for i = 1 to 10_000_000 do
    Queue.add i q
  done *)
