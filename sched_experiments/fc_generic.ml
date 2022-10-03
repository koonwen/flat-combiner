type publication_record =
  { mutable active : bool (* ; mutable request : unit -> 'a option *)
  ; mutable request : unit -> (int * int) option (* ; mutable result : 'a option *)
  ; mutable result : (int * int) option
  ; mutable pending : bool
  ; mutable age : int
  }
[@@deriving show]

type ('a, 'b) t =
  { global_lock : Mutex.t
  ; mutable count : int
  ; mutable ds : 'b
  ; pub_list : publication_record List.t Atomic.t
  ; task_records : (int, publication_record) Hashtbl.t
      (* This may increase indefinetly to accomodate n tasks *)
      (* Optimize this by making sure each id for a task is kept in a Domain Local array *)
  }

let init_pub_rec () =
  { active = false; request = (fun () -> None); result = None; pending = false; age = 0 }
;;

(* let create ?(tasks = Domain.recommended_domain_count) ds = *)
let create ?(tasks = 8) ds =
  { global_lock = Mutex.create ()
  ; count = 0
  ; pub_list = Atomic.make []
  ; ds
  ; task_records = Hashtbl.create tasks
  }
;;

(* let pp p x = Format.pp_print_string p (show x) *)

let rec scan_combine_apply id t pr =
  (* Printf.printf "publist_length = %d%!" (List.length (t.pub_list |> Atomic.get)); *)
  if not pr.pending
  then pr.result
  else if Mutex.try_lock t.global_lock
  then (
    (* The task needs to return to the same domain *)
    (* Printf.printf "(%d) Combiner\n%!" id; *)
    let domain_id1 = (Domain.self () :> int) in
    Schedulr.Scheduler.yield ~lock_holding:true ();
    let domain_id2 = (Domain.self () :> int) in
    assert (domain_id1 = domain_id2);
    t.count <- t.count + 1;
    List.iter
      (fun pr ->
        if pr.pending
        then (
          (* print_endline "Exec"; *)
          (* Printf.printf "%s\n" (show_publication_record pr); *)
          pr.result <- pr.request ();
          pr.age <- t.count;
          pr.pending <- false))
      (Atomic.get t.pub_list);
    Mutex.unlock t.global_lock;
    let res = pr.result in
    pr.result <- None;
    res)
  else (
    let cnt = ref 0 in
    while pr.pending && !cnt <= 1000 do
      Schedulr.Scheduler.yield ();
      Domain.cpu_relax ();
      incr cnt
    done;
    scan_combine_apply id t pr)
;;

let apply id t request =
  let pr =
    try Hashtbl.find t.task_records id with
    | Not_found ->
      let new_pr = init_pub_rec () in
      Hashtbl.add t.task_records id new_pr;
      new_pr
  in
  pr.request <- request;
  pr.pending <- true;
  if not pr.active
  then (
    pr.active <- true;
    while
      not
      @@
      let ls = Atomic.get t.pub_list in
      Atomic.compare_and_set t.pub_list ls (pr :: ls)
    do
      Domain.cpu_relax ()
    done);
  (* Printf.printf "publist_length = %d\n%!" (List.length (t.pub_list |> Atomic.get)); *)
  (* Printf.printf "id (%d) task records = %d\n" id (t.task_records |> Hashtbl.length); *)
  scan_combine_apply id t pr
;;
