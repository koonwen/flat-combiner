module FC_Queue = struct
  let num_domains = 2
  let _q = Queue.create ()
  let _fcq = Fc_generic_domains.create ~data_structure:_q ~num_domains

  let enq v =
    match
      Fc_generic_domains.apply _fcq (fun () ->
        Queue.push v _q;
        None)
    with
    | None -> ()
    | _ -> failwith "Enq Impossible"
  ;;

  let deq () = Fc_generic_domains.apply _fcq (fun () -> Queue.take_opt _q)
end

let rec enqueuer lo hi =
  if lo <= hi
  then (
    let _id = Domain.self () in
    FC_Queue.enq lo;
    enqueuer (lo + 1) hi)
  else ()
;;

let dequeuer n =
  let acc = ref [] in
  let rec aux n =
    if n > 0
    then (
      (match FC_Queue.deq () with
       | Some v -> acc := v :: !acc
       | None -> ());
      aux (n - 1))
    else ()
  in
  aux n;
  !acc
;;

(* let () =
  let t1 = Domain.spawn (fun () -> enqueuer 100_000) in
  let t2 = Domain.spawn (fun () -> dequeuer 100_000) in
  Domain.join t1;
  Domain.join t2 |> List.length |> print_int
;; *)

(* let test_sequential_consistency n =
  let mid = n / 2 in
  let e1 = Domain.spawn (fun () -> enqueuer 1 mid) in
  let e2 = Domain.spawn (fun () -> enqueuer (mid + 1) n) in
  Domain.join e1;
  Domain.join e2;
  (* FC_Queue._q |> Queue.length |> print_int; *)
  FC_Queue._q |> Queue.iter (Printf.printf "%d ")
;;

let () = test_sequential_consistency 10000 *)