module FC_Queue = struct
  let _q = Queue.create ()
  let _fcq = Fc_generic_threads.create ~data_structure:_q ~num_threads:4

  let enq v =
    match
      Fc_generic_threads.apply _fcq (fun () ->
        Queue.push v _q;
        None)
    with
    | None -> ()
    | _ -> failwith "Enq Impossible"
  ;;

  let deq () =
    Fc_generic_threads.apply _fcq (fun () ->
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
  List.iter (fun v -> Printf.printf "%d " v) !acc;
  print_newline ()
;;

let () =
  let t1 = Thread.create enqueuer 100 in
  let t2 = Thread.create dequeuer 100 in
  let t3 = Thread.create enqueuer 100 in
  let t4 = Thread.create dequeuer 100 in
  List.iter (fun t -> Thread.join t) [ t1; t2; t3; t4 ];
  ()
;;
