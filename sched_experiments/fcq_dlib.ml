(* open Fcq
module T = Domainslib.Task

(* This enqueuer runs in any order, we may not test for sequential consistency *)
let para_enq pool start finish =
  T.parallel_for
    ~start
    ~finish
    ~body:(fun i ->
      let id = get_domain_id () in
      enq ~id (id, i))
    pool
;;

let para_seq_enq pool start finish n =
  T.parallel_for
    ~start
    ~finish
    ~body:(fun _ ->
      let id = get_domain_id () in
      enqueuer ~id 1 n)
    pool
;;

let para_deq pool start finish =
  let acc = Atomic.make [] in
  T.parallel_for
    ~start
    ~finish
    ~body:(fun _ ->
      let id = get_domain_id () in
      let _, res =
        deq ~id ()
        |> function
        | Some v -> v
        | None -> failwith "Error"
      in
      let flag = ref true in
      while !flag do
        let old_l = Atomic.get acc in
        if Atomic.compare_and_set acc old_l ((id, res) :: old_l) then flag := false
      done)
    pool;
  Atomic.get acc |> List.rev
;;

let pp_int_int_queue = Fmt.(queue ~sep:semi (parens (pair ~sep:comma int int)))
let pp_int_int_seq = Fmt.(seq ~sep:semi (parens (pair ~sep:comma int int)))
let pp_int_list = Fmt.(list ~sep:semi int)

let test_enq_sequential_consistency pool n =
  T.run pool (fun () -> para_seq_enq pool 1 num_domains n);
  (* let tbl = Util.distribute num_domains (_q |> Queue.to_seq) in
  Hashtbl.iter (fun key v -> Fmt.pr "(Domain %d)\n\n%a\n\n" key pp_int_list v) tbl;
  Fmt.(pr "%a\n" pp_int_int_queue _q); *)
  (* Check length of Queue *)
  assert (_q |> Queue.length = n * num_domains);
  (* Check that elements are unique *)
  assert (_q |> Queue.to_seq |> Util.check_elements n num_domains);
  (* Check sequential consistency *)
  assert (_q |> Queue.to_seq |> Util.check_order num_domains)
;;

let test_deq_sequential_consistency pool n =
  Util.populate (get_domain_id ()) 1 n _q;
  let res_seq = T.run pool (fun () -> para_deq pool 1 n) |> List.to_seq in
  (* let tbl = Util.distribute num_domains res_seq in
  Hashtbl.iter (fun key v -> Fmt.pr "(Domain %d)\n\n%a\n\n" key pp_int_list v) tbl;
  Fmt.(pr "%a\n" pp_int_int_seq res_seq); *)
  (* Check correct number of elements dequeued *)
  (* assert (Seq.length res_seq = n); *)
  assert (Util.length res_seq = n);
  (* Check that elements are unique *)
  assert (Util.check_unique res_seq);
  (* Check sequential consistency *)
  assert (_q |> Queue.to_seq |> Util.check_order num_domains)
;;

let () =
  let pool = T.setup_pool ~num_additional_domains:num_domains () in
  let n = 1_000_000 in
  test_enq_sequential_consistency pool n;
  clear ~id:0 ();
  test_deq_sequential_consistency pool n;
  T.teardown_pool pool
;; *)
