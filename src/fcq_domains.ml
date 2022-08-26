module FC_Queue = struct
  let _q = Queue.create ()
  let _fcq = Fc_generic_domains.create ~data_structure:_q ~num_domains:2

  let enq v =
    match
      Fc_generic_domains.apply _fcq (fun () ->
        Queue.push v _q;
        None)
    with
    | None -> ()
    | _ -> failwith "Enq Impossible"
  ;;

  let deq () =
    Fc_generic_domains.apply _fcq (fun () ->
      try Queue.pop _q |> Option.some with
      | _ -> None)
  ;;
end

let rec enqueuer i =
  if i > 0
  then (
    let _id = Thread.self () |> Thread.id in
    FC_Queue.enq i;
    enqueuer (i - 1))
  else ()
;;

let dequeuer i =
  let acc = ref [] in
  let rec aux i =
    if i > 0
    then (
      (match FC_Queue.deq () with
       | Some v -> acc := v :: !acc
       | None -> ());
      aux (i - 1))
    else ()
  in
  aux i;
  !acc
;;

let () =
  let t1 = Domain.spawn (fun () -> enqueuer 100_000) in
  let t2 = Domain.spawn (fun () -> dequeuer 100_000) in
  Domain.join t1;
  Domain.join t2 |> List.length |> print_int
;;
