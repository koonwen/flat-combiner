open Queues
open Util

let q = Lock_queue.init ()

let rec enqueuer lo hi =
  if lo <= hi
  then (
    let _id = Domain.self () in
    Lock_queue.enqueue q lo;
    enqueuer (lo + 1) hi)
  else ()
;;

let _dequeuer n =
  let acc = ref [] in
  let rec aux n =
    if n > 0
    then (
      (match Lock_queue.dequeue q with
       | Some v -> acc := v :: !acc
       | None -> ());
      aux (n - 1))
    else ()
  in
  aux n;
  !acc
;;

let test_sequential_consistency n =
  let mid = n / 2 in
  let e1 = Domain.spawn (fun () -> enqueuer 1 mid) in
  let e2 = Domain.spawn (fun () -> enqueuer (mid + 1) n) in
  Domain.join e1;
  Domain.join e2;
  (* Check length of Queue *)
  assert (q.queue |> Queue.length = n);
  (* Check that elements are consistent *)
  assert (q.queue |> Queue.to_seq |> check_elements 1 n);
  (* Check sequential consistency *)
  assert (q.queue |> Queue.to_seq |> check_order n)
;;

let () = test_sequential_consistency 1_000_000