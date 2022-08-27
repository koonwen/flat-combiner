type 'a publication_record =
  { mutable active : bool
  ; mutable request : unit -> 'a Option.t
  ; mutable result : 'a Option.t
  ; mutable pending : bool
  ; mutable age : int
  }

type ('a, 'b) t =
  { global_lock : Mutex.t
  ; count : int
  ; mutable pub_list : 'a publication_record List.t
  ; mutable ds : 'b
      (* We cannot use an array because in 4.12.0+domains+effects, the Thread ID isn't consistent in increasing order. *)
  ; thread_records : (Thread.t, 'a publication_record) Hashtbl.t
  }

let init_pub_rec () =
  { active = false; request = (fun () -> None); result = None; pending = true; age = 0 }
;;

let create ~data_structure:ds ~num_threads =
  { global_lock = Mutex.create ()
  ; count = 0
  ; pub_list = []
  ; ds
  ; thread_records = Hashtbl.create num_threads
  }
;;

let rec scan_combine_apply t pr =
  if not pr.pending
  then pr.result
  else if Mutex.try_lock t.global_lock
  then (
    List.iter
      (fun pr ->
        if pr.pending
        then (
          pr.result <- pr.request ();
          pr.age <- pr.age + 1;
          pr.pending <- false))
      t.pub_list;
    Mutex.unlock t.global_lock;
    pr.result)
  else scan_combine_apply t pr
;;

let apply t request =
  let id = Thread.self () in
  let pr =
    (* t.thread_records.(id) in *)
    try Hashtbl.find t.thread_records id with
    | Not_found ->
      let new_pr = init_pub_rec () in
      Hashtbl.add t.thread_records id new_pr;
      new_pr
  in
  if not pr.active
  then (
    t.pub_list <- pr :: t.pub_list;
    pr.active <- true);
  pr.result <- None;
  pr.request <- request;
  pr.pending <- true;
  scan_combine_apply t pr
;;

let pp_pr pr =
  Printf.sprintf
    {|
  {
    active : %b,
    pending : %b,
    age : %d
  }
  |}
    pr.active
    pr.pending
    pr.age
;;
