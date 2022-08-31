open Queues.Mpmc_queue
open Util

let get_queue q =
  let l = q.head |> Atomic.get in
  let r = q.tail |> Atomic.get in
  let s = ref Seq.empty in
  for i = r - 1 downto l do
    let res =
      match q.array.(i) |> Atomic.get with
      | Some v -> v
      | None -> failwith "Impossible"
    in
    s := Seq.cons res !s
  done;
  !s
;;

let rec populate id lo hi t =
  if lo <= hi
  then (
    enqueue t (id, lo);
    populate id (lo + 1) hi t)
;;

let test_enq_sequential_consistency n =
  let num_domains = Domain.recommended_domain_count in
  let div = n / num_domains in
  let d_arr = d_spawner (fun () -> enqueuer 1 div) ~num_domains in
  Array.iter Domain.join d_arr;
  (* Check length of Queue *)
  let q_seq = get_queue !_q in
  assert (Atomic.get !_q.tail - Atomic.get !_q.head = n);
  (* Check that elements are unique *)
  assert (q_seq |> check_elements n num_domains);
  (* Check sequential consistency *)
  assert (q_seq |> check_order num_domains)
;;

let test_deq_sequential_consistency n =
  let num_domains = Domain.recommended_domain_count in
  let div = n / num_domains in
  populate (Domain.self ()) 1 n !_q;
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

let test_mpmc_queue_enq n () =
  recreate ();
  Alcotest.(check unit) "Passed" () (test_enq_sequential_consistency n)
;;

let test_mpmc_queue_deq n () =
  recreate ();
  Alcotest.(check unit) "Passed" () (test_deq_sequential_consistency n)
;;
