open Queues
open Util

let test_enq_sequential_consistency n =
  let num_domains = Domain.recommended_domain_count in
  let div = n / num_domains in
  let d_arr = d_spawner (fun () -> Lock_queue.enqueuer 1 div) ~num_domains in
  Array.iter Domain.join d_arr;
  (* Check length of Queue *)
  assert (Lock_queue._q.queue |> Queue.length = n);
  (* Check that elements are unique *)
  assert (Lock_queue._q.queue |> Queue.to_seq |> check_elements n num_domains);
  (* Check sequential consistency *)
  assert (Lock_queue._q.queue |> Queue.to_seq |> check_order num_domains)
;;

let test_deq_sequential_consistency n =
  let num_domains = Domain.recommended_domain_count in
  let div = n / num_domains in
  populate (Domain.self ()) 1 n Lock_queue._q.queue;
  let d_arr = d_spawner (fun () -> Lock_queue.dequeuer div) ~num_domains in
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

let test_lock_queue_enq n () =
  Queue.clear Lock_queue._q.queue;
  Alcotest.(check unit) "Passed" () (test_enq_sequential_consistency n)
;;

let test_lock_queue_deq n () =
  Queue.clear Lock_queue._q.queue;
  Alcotest.(check unit) "Passed" () (test_deq_sequential_consistency n)
;;
