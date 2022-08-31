open Util
open Fcq_domains

let test_enq_sequential_consistency n =
  let num_domains = FC_Queue.num_domains in
  let div = n / num_domains in
  let d_arr = d_spawner (fun () -> enqueuer 1 div) ~num_domains in
  Array.iter Domain.join d_arr;
  (* Check length of Queue *)
  assert (FC_Queue._q |> Queue.length = n);
  (* Check that elements are unique *)
  assert (FC_Queue._q |> Queue.to_seq |> check_elements n num_domains);
  (* Check sequential consistency *)
  assert (FC_Queue._q |> Queue.to_seq |> check_order num_domains)
;;

let test_deq_sequential_consistency n =
  let num_domains = FC_Queue.num_domains in
  let div = n / num_domains in
  populate (Domain.self ()) 1 n FC_Queue._q;
  let d_arr = d_spawner (fun () -> dequeuer div) ~num_domains in
  let res_arr = Array.map Domain.join d_arr in
  let res_seq =
    Array.fold_left (fun acc l -> Seq.append (List.to_seq l) acc) Seq.empty res_arr
  in
  (* Check correct number of elements dequeued *)
  assert (Seq.length res_seq = n);
  (* Check that elements are unique *)
  assert (check_elements n 1 res_seq);
  (* Check sequential consistency *)
  assert (Array.for_all check_descending res_arr)
;;

let test_fcq_domains_enq n () =
  Queue.clear FC_Queue._q;
  Alcotest.(check unit) "Passed" () (test_enq_sequential_consistency n)
;;

let test_fcq_domains_deq n () =
  Queue.clear FC_Queue._q;
  Alcotest.(check unit) "Passed" () (test_deq_sequential_consistency n)
;;
