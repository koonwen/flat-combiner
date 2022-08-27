let check_elements n seq =
  let rec aux acc = function
    | Seq.Nil -> true
    | Seq.Cons (hd, t) ->
      if 1 <= hd && hd <= n && not (List.mem hd acc)
      then aux (hd :: acc) (t ())
      else (
        Printf.printf "(%d)" hd;
        false)
  in
  aux [] (seq ())
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
  let open Fcq_domains in
  let mid = n / 2 in
  let e1 = Domain.spawn (fun () -> enqueuer 1 mid) in
  let e2 = Domain.spawn (fun () -> enqueuer (mid + 1) n) in
  Domain.join e1;
  Domain.join e2;
  (* FC_Queue._q |> Queue.length |> print_int; *)
  (* FC_Queue._q |> Queue.iter (Printf.printf "%d "); *)
  (* Check length of Queue *)
  assert (FC_Queue._q |> Queue.length = n);
  (* Check that elements are consistent *)
  assert (FC_Queue._q |> Queue.to_seq |> check_elements n);
  (* Check sequential consistency *)
  assert (FC_Queue._q |> Queue.to_seq |> check_order n)
;;

let () = test_sequential_consistency 10000