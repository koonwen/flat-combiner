open! Logs

module type Content = sig
  type t

  val to_string : t -> string
end

module PublicationRecord (T : Content) = struct
  type elt = T.t

  (* Instead of opcode, consider using a thunk *)
  type opcode =
    | Enq
    | Deq

  let op_to_string = function
    | Enq -> "Enq"
    | Deq -> "Deq"
  ;;

  type publication =
    { opcode : opcode
    ; parameters : elt Option.t
    ; mutable age : int
    ; mutable result : elt Option.t
    ; mutable pending : bool
    }

  type t = publication ref Queue.t

  let create () : t = Queue.create ()

  let add_publication ~opcode ?(parameters = None) pr =
    let new_pub = ref { opcode; parameters; age = 0; result = None; pending = true } in
    Queue.add new_pub pr;
    new_pub
  ;;

  let install_result result (pub : publication ref) =
    !pub.result <- Some result;
    !pub.age <- !pub.age + 1;
    !pub.pending <- false
  ;;

  let pending t = t.pending
  let param t = t.parameters |> Option.get
  let result t = t.result

  (* Why doesn't this need to be a ref *)
  let result_available pub = pub.pending <- false
  let t_to_seq (pr : t) = Queue.to_seq pr

  let res_to_string = function
    | Some v -> Printf.sprintf "Some(%s)" (T.to_string v)
    | None -> "None"
  ;;

  let print_res = function
    | Some v -> Printf.printf "Some(%s)" (T.to_string v)
    | None -> Printf.printf "None"
  ;;

  let pp pub =
    Printf.sprintf
      {|
      {
        opcode : %s;
        parameters : %s
        age : %d;
        result : %s
        pending : %b
      }
      |}
      (op_to_string pub.opcode)
      (pub.parameters |> res_to_string)
      pub.age
      (pub.result |> res_to_string)
      pub.pending
  ;;
end

module FC_queue (T : Content) = struct
  (* Needs to initialize some domains *)

  module PR = PublicationRecord (T)

  type elt = T.t

  (* let pub_rec_arr = Array.make 4 (PR.create ()) *)

  type t =
    { mutable pub_list : PR.t List.t
    ; mutable g_lock : bool Atomic.t (*Mutex*)
    ; mutable count : int Atomic.t
    ; queue : elt Queue.t
    }

  let main_thread_pl = PR.create ()

  let create () =
    { pub_list = [ main_thread_pl ]
    ; g_lock = Atomic.make true
    ; count = Atomic.make 0
    ; queue = Queue.create ()
    }
  ;;

  let contend t : bool = true

  let scan_combine_apply t =
    Logs.info (fun m -> m "Start Scan_Combine_Apply");
    let execute pub =
      if PR.pending !pub
      then (
        match !pub.opcode with
        | Enq ->
          let param = PR.param !pub in
          Logs.info (fun m -> m "Enqueuing");
          Queue.push param t.queue;
          PR.result_available !pub
        | Deq ->
          Logs.info (fun m -> m "Dequeuing");
          let res = Queue.pop t.queue in
          PR.install_result res pub)
    in
    let s =
      List.fold_left (fun acc pr -> Seq.append acc (PR.t_to_seq pr)) Seq.empty t.pub_list
    in
    Seq.iter (fun p -> execute p) s;
    Logs.info (fun m -> m "End Scan_Combine_Apply")
  ;;

  let traverse_publist t =
    let s =
      List.fold_left (fun acc pr -> Seq.append acc (PR.t_to_seq pr)) Seq.empty t.pub_list
    in
    Seq.iter (fun p -> Logs.debug (fun m -> m "%a" Format.pp_print_string (PR.pp !p))) s
  ;;

  let log_queue t =
    t.queue
    |> Queue.to_seq
    |> Seq.iter (fun v ->
         Logs.debug (fun m -> m "%a" Format.pp_print_string (T.to_string v)))
  ;;

  (* let res = T.to_string v in
           m res)) *)

  let enqueue t v =
    (* let id = Thread.id in *)
    let pr = main_thread_pl in
    let pub = PR.add_publication ~opcode:Enq ~parameters:(Some v) pr in
    if contend t then scan_combine_apply t;
    while PR.pending !pub do
      ()
    done
  ;;

  let dequeue t =
    let rec _check_res pub =
      print_endline "Checking result";
      if PR.pending pub
      then (
        Printf.printf "Spinning\n";
        _check_res pub)
      else pub.result |> Option.get
    in
    (* let id = Thread.id in *)
    let pr = main_thread_pl in
    let pub = PR.add_publication ~opcode:Deq pr in
    if contend t then scan_combine_apply t;
    _check_res !pub
  ;;
end

(* Age doesn't increase? *)