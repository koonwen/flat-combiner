open Util
open Fcq_domains

let test_sequential_consistency n =
  let mid = n / 2 in
  let e1 = Domain.spawn (fun () -> enqueuer 1 mid) in
  let e2 = Domain.spawn (fun () -> enqueuer (mid + 1) n) in
  Domain.join e1;
  Domain.join e2;
  (* FC_Queue._q |> Queue.length |> print_int; *)
  (* FC_Queue._q |> Queue.iter (Printf.printf "%d "); *)
  (* Check length of Queue *)
  assert (FC_Queue._q |> Queue.length = n);
  (* Check that elements are unique *)
  assert (FC_Queue._q |> Queue.to_seq |> check_elements 1 n);
  (* Check sequential consistency *)
  assert (FC_Queue._q |> Queue.to_seq |> check_order n)
;;

let test_deq_sequential_consistency n =
  let mid = n / 2 in
  Queue.clear FC_Queue._q;
  populate 1 n FC_Queue._q;
  let d1 = Domain.spawn (fun () -> dequeuer mid) in
  let d2 = Domain.spawn (fun () -> dequeuer (n - mid)) in
  let d1_acc = Domain.join d1 in
  let d2_acc = Domain.join d2 in
  (* Check length of items dequeued *)
  assert (List.length d1_acc + List.length d2_acc = n);
  (* Check that elements are unique *)
  assert (check_elements 1 n (List.to_seq (d1_acc @ d2_acc)));
  (* Check sequential consistency *)
  assert (check_descending d1_acc && check_descending d2_acc)
;;

let () =
  test_sequential_consistency 1_000_000;
  test_deq_sequential_consistency 1_000_000
;;
