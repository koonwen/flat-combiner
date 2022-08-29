open Util
open Fcq_threads

let test_enq_sequential_consistency n =
  let mid = n / 2 in
  let e1 = Thread.create (fun (lo, hi) -> enqueuer_v3 lo hi) (1, mid) in
  let e2 = Thread.create (fun (lo, hi) -> enqueuer_v3 lo hi) (mid + 1, n) in
  Thread.join e1;
  Thread.join e2;
  (* Check length of Queue *)
  assert (FC_Queue._q |> Queue.length = n);
  (* Check that elements are consistent *)
  assert (FC_Queue._q |> Queue.to_seq |> check_elements 1 n);
  (* Check sequential consistency *)
  assert (FC_Queue._q |> Queue.to_seq |> check_order n)
;;

let test_deq_sequential_consistency n =
  let mid = n / 2 in
  Queue.clear FC_Queue._q;
  populate 1 n FC_Queue._q;
  (* Here we don't need to synchronize on the list because there isn't true parallelism going on. *)
  let d1_acc = ref [] in
  let d2_acc = ref [] in
  let d1 = Thread.create (fun (n, acc) -> dequeuer_v3 n acc) (mid, d1_acc) in
  let d2 = Thread.create (fun (n, acc) -> dequeuer_v3 n acc) (n - mid, d2_acc) in
  Thread.join d1;
  Thread.join d2;
  (* Check length of items dequeued *)
  assert (List.length !d1_acc + List.length !d2_acc = n);
  (* Check that elements are unique *)
  assert (check_elements 1 n (List.to_seq (!d1_acc @ !d2_acc)));
  (* Check sequential consistency *)
  assert (check_descending !d1_acc && check_descending !d2_acc)
;;

let () =
  test_enq_sequential_consistency 1_000_000;
  test_deq_sequential_consistency 1_000_000
;;
