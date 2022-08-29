open Queues
open Util

let q = Mpmc_queue.init ()

let rec enqueuer lo hi =
  if lo <= hi
  then (
    let _id = Domain.self () in
    Mpmc_queue.enqueue q lo;
    enqueuer (lo + 1) hi)
  else ()
;;

let dequeuer n =
  let acc = ref [] in
  let rec aux n =
    if n > 0
    then (
      (match Mpmc_queue.dequeue q with
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
  let res = dequeuer n |> List.rev in
  assert (res |> List.length = n);
  (* Check that elements are consistent *)
  assert (List.to_seq res |> check_elements 1 n);
  (* Check sequential consistency *)
  assert (List.to_seq res |> check_order n)
;;

let () =
  (* Mpmc_queue._test_1 () *)
  test_sequential_consistency 10_000
;;
