module FC_Queue = struct
  let num_threads = 2
  let _q = Queue.create ()
  let _fcq = Fc_generic_threads.create ~data_structure:_q ~num_threads

  let enq v =
    match
      Fc_generic_threads.apply _fcq (fun () ->
        Queue.push v _q;
        None)
    with
    | None -> ()
    | _ -> failwith "Enq Impossible"
  ;;

  let deq () = Fc_generic_threads.apply _fcq (fun () -> Queue.take_opt _q)
end

(* Comments: 
    The native threading facilty in OCaml offers only concurrency and not parallelism because the global runtime lock where only one thread is allowed to run OCaml code at a time. On top of this, the functions executed by the threads aren't pre-empted therefore we have to manually insert yields so that we can have interleaving. The order in which two freshly created threads are executed are entirely up to the scheduler.
*)

(* Attempt 1 *)
let rec enqueuer_v1 i =
  if i > 0
  then (
    let _id = Thread.self () |> Thread.id in
    FC_Queue.enq i;
    enqueuer_v1 (i - 1))
  else ()
;;

let dequeuer_v1 i =
  let acc = ref [] in
  let rec aux i =
    if i > 0
    then (
      (match FC_Queue.deq () with
       | Some v -> acc := v :: !acc
       | None -> ());
      aux (i - 1))
    else ()
  in
  aux i;
  List.iter (fun v -> Printf.printf "%d " v) !acc;
  print_newline ()
;;

let prog1 () =
  let t1 = Thread.create enqueuer_v1 100 in
  let t2 = Thread.create dequeuer_v1 100 in
  Thread.join t1;
  Thread.join t2
;;

(* Analysis:
   The problem with this implementation is that within each newly created thread, we do not yield at each enqueue, because of this reason, we do not have the desired interleaving. We either have the enqueuer executing first and dumping it's values entirely in the queue, or the dequeuer trying to dequeue the empty list.
*)

(* Attempt 2 *)
let rec enqueuer_v2 lo hi =
  if lo > hi
  then ()
  else (
    Thread.create FC_Queue.enq lo |> Thread.join;
    enqueuer_v2 (lo + 1) hi)
;;

let rec dequeuer_v2 lo hi =
  if lo > hi
  then ()
  else (
    Thread.create
      (fun () ->
        match FC_Queue.deq () with
        | None -> Printf.printf "Empty"
        | Some v -> Printf.printf "%d " v)
      ()
    |> Thread.join;
    dequeuer_v2 (lo + 1) hi)
;;

let prog2 () =
  let t1 = Thread.create (fun (lo, hi) -> enqueuer_v2 lo hi) (0, 10) in
  let t2 = Thread.create (fun (lo, hi) -> dequeuer_v2 lo hi) (0, 10) in
  Thread.join t1;
  Thread.join t2
;;

(* let () =
  prog1 ();
  print_endline " --------------- ";
  prog2 ();
  print_newline ()
;; *)

(* Analysis 
   The problem with this implementation is that we create fresh threads every cycle instead of reusing the old threads. This is problematic because in our initialization of the FCQ, we specify the number of threads that we are going to use in the program which restricts the number of threads we can create to access the thread. We observe an index out of bounds in this implementation (This only happens when the thread_record was implemented as an array. Now that it is a Hashtable, prog2 will work but creates a huge and inefficient thread_record and pub_list) *)

(** [FC_Queue.enqueuer_v3 lo hi] enqueues the range of values from [lo] (inclusive) to [hi] (inclusive) in a concurrent fashion *)
let rec enqueuer_v3 lo hi =
  if lo <= hi
  then (
    let _id = Thread.self () |> Thread.id in
    FC_Queue.enq lo;
    Thread.yield ();
    enqueuer_v3 (lo + 1) hi)
;;

(** [FC_Queue.dequeuer_v3 n acc] dequeues n values from the queue a concurrent fashion and returns the list of values in right to left order *)
let dequeuer_v3 n acc =
  let rec aux n =
    if n > 0
    then (
      (match FC_Queue.deq () with
       | Some v -> acc := v :: !acc
       | None -> ());
      Thread.yield ();
      aux (n - 1))
  in
  aux n;
  !acc
;;

let prog3 () =
  let t1 = Thread.create (fun (lo, hi) -> enqueuer_v3 lo hi) (0, 10) in
  let t2 = Thread.create dequeuer_v3 10 in
  Thread.join t1;
  Thread.join t2
;;
