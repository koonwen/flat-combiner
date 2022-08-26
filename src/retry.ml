(* module PublicationRecord = struct
  type elt

  type request =
    | Thunk_unit of (unit -> unit)
    | Thunk_elt of (unit -> elt)

  type publication_record =
    { active : bool
    ; request : request Option.t
    ; result : elt Option.t
    }
end *)

module FC
  (*(DataStructure : sig
  type 'a t

  val create : unit -> 'a t
end)*) =
struct
  type 'a publication_record =
    { mutable active : bool
    ; mutable request : unit -> unit
    ; mutable result : 'a Option.t
    ; mutable pending : bool
    ; mutable age : int
    }

  type 'a t =
    { global_lock : Mutex.t
    ; count : int
    ; mutable pub_list : 'a publication_record List.t
    ; (* ; ds : 'a DataStructure.t *)
      mutable ds : 'a Queue.t
    ; thread_records : 'a publication_record Array.t
    }

  (* let create num_threads () =
    let init_pub_rec = { active = false; request = None } in
    { global_lock = Mutex.create ()
    ; count = 0
    ; pub_list = []
    ; ds = DataStructure.create ()
    ; thread_records = Array.make num_threads init_pub_rec
    }
  ;; *)

  let create_queue () =
    let init_pub_rec =
      { active = false; request = (fun () -> ()); result = None; pending = true; age = 0 }
    in
    { global_lock = Mutex.create ()
    ; count = 0
    ; pub_list = []
    ; ds = Queue.create ()
    ; thread_records = Array.make 4 init_pub_rec
    }
  ;;

  let update_record pub ~active ~pending ~result ~request =
    pub.active <- active;
    pub.pending <- pending;
    pub.result <- result;
    pub.request <- request
  ;;

  let rec scan_combine_apply t pr =
    if not pr.pending
    then pr.result
    else if Mutex.try_lock t.global_lock
    then (
      List.iter
        (fun pr ->
          pr.request ();
          pr.age <- pr.age + 1;
          pr.pending <- false)
        t.pub_list;
      Mutex.unlock t.global_lock;
      pr.result)
    else scan_combine_apply t pr
  ;;

  let _enq_op t v () =
    try Queue.push v t.ds with
    | e -> Printexc.to_string e |> print_endline
  ;;

  let enq t v =
    let pr = t.thread_records.(Thread.id (Thread.self ())) in
    if not pr.active
    then (
      t.pub_list <- pr :: t.pub_list;
      pr.active <- true);
    pr.pending <- true;
    pr.result <- None;
    pr.request <- _enq_op t v;
    match scan_combine_apply t pr with
    | None -> ()
    | _ -> failwith "Impossible enq"
  ;;

  let _deq_op t pr () =
    let try_deq () =
      let res = Queue.pop t.ds in
      pr.result <- Some res
    in
    try try_deq () with
    | e -> ()
  ;;

  let deq t =
    let pr = t.thread_records.(Thread.id (Thread.self ())) in
    if not pr.active
    then (
      t.pub_list <- pr :: t.pub_list;
      pr.active <- true);
    pr.pending <- true;
    pr.result <- None;
    pr.request <- _deq_op t pr;
    match scan_combine_apply t pr with
    | Some v -> v
    | _ -> failwith "Empty Queue"
  ;;

  let cleanup t = failwith ""
  let traverse t = t.ds |> Queue.iter (fun v -> Printf.printf "%d\n" v)
end

let enqueuer fcq =
  for i = 0 to 10 do
    FC.enq fcq i;
    Printf.printf "Enq %d success\n" i
  done
;;

let dequeuer fcq =
  let l = ref [] in
  for _i = 0 to 10 do
    let res =
      try FC.deq fcq with
      | e ->
        print_endline "Empty Queue";
        -1
    in
    if res > -1
    then (
      Printf.printf "dequeued %d\n" res;
      l := res :: !l)
  done;
  List.iter (fun v -> Printf.printf "%d " v) !l
;;

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()
;;

(* Command line interface *)

open Cmdliner

let setup_log = Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let prog () =
  let fcq = FC.create_queue () in
  let t1 = Thread.create enqueuer fcq in
  let t2 = Thread.create dequeuer fcq in
  Thread.join t1;
  Thread.join t2
;;

let main prog = Cmd.(v (info "flat combiner") Term.(const prog $ setup_log))

let () =
  match Cmd.eval (main prog) with
  | _ -> exit (if Logs.err_count () > 0 then 1 else 0)
;;