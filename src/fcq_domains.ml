module FC_Queue = struct
  let num_domains = Domain.recommended_domain_count
  let _q = Queue.create ()
  let _fcq = Fc_generic_domains.create ~data_structure:_q ~num_domains

  let enq v =
    match
      Fc_generic_domains.apply _fcq (fun () ->
        Queue.push v _q;
        None)
    with
    | None -> ()
    | _ -> failwith "enq Impossible"
  ;;

  let deq () = Fc_generic_domains.apply _fcq (fun () -> Queue.take_opt _q)

  let clear () =
    match
      Fc_generic_domains.apply _fcq (fun () ->
        Queue.clear _q;
        None)
    with
    | None -> ()
    | _ -> failwith "clear Impossible"
  ;;
end

let rec enqueuer lo hi =
  if lo <= hi
  then (
    let id = Domain.self () in
    FC_Queue.enq (id, lo);
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
