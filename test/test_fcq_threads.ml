let check_elements n seq =
  let module IntSet = Set.Make (Int) in
  let rec aux set = function
    | Seq.Nil -> true
    | Seq.Cons (hd, t) ->
      if 1 <= hd && hd <= n && not (IntSet.mem hd set)
      then aux (IntSet.add hd set) (t ())
      else false
  in
  aux IntSet.empty (seq ())
;;

let check_order n seq =
  let l, r = 1, n / 2 in
  let rec aux l r = function
    | Seq.Nil -> true
    | Seq.Cons (hd, t) ->
      if hd >= l && hd >= r
      then aux l hd (t ())
      else if hd >= l
      then aux hd r (t ())
      else false
  in
  aux l r (seq ())
;;

let test_sequential_consistency n =
  let open Fcq_threads in
  let mid = n / 2 in
  let e1 = Thread.create (fun (lo, hi) -> enqueuer_v3 lo hi) (1, mid) in
  let e2 = Thread.create (fun (lo, hi) -> enqueuer_v3 lo hi) (mid + 1, n) in
  Thread.join e1;
  Thread.join e2;
  (* Check length of Queue *)
  assert (FC_Queue._q |> Queue.length = n);
  (* Check that elements are consistent *)
  assert (FC_Queue._q |> Queue.to_seq |> check_elements n);
  (* Check sequential consistency *)
  assert (FC_Queue._q |> Queue.to_seq |> check_order n)
;;

(* let left, right = FC_Queue._q |> Queue.to_seq |> Seq.partition (fun i -> i <= mid) in
  Seq.iter (fun v -> Printf.printf "%d " v) left *)

let () = test_sequential_consistency 1_000_000