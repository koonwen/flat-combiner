type 'a publication_record =
  { mutable active : bool
  ; mutable request : unit -> 'a Option.t
  ; mutable result : 'a Option.t
  ; mutable pending : bool
  ; mutable age : int
  }

type ('a, 'b) t =
  { global_lock : Mutex.t
  ; mutable count : int
  ; mutable ds : 'b
  ; pub_list : 'a publication_record List.t Atomic.t
  ; domain_records : (Domain.id, 'a publication_record) Hashtbl.t
  }

let init_pub_rec () =
  { active = false; request = (fun () -> None); result = None; pending = false; age = 0 }
;;

let create ~data_structure:ds ~num_domains =
  { global_lock = Mutex.create ()
  ; count = 0
  ; pub_list = Atomic.make []
  ; ds
  ; domain_records = Hashtbl.create num_domains
  }
;;

let rec scan_combine_apply t pr =
  if not pr.pending
  then pr.result
  else if Mutex.try_lock t.global_lock
  then (
    t.count <- t.count + 1;
    List.iter
      (fun pr ->
        if pr.pending
        then (
          pr.result <- pr.request ();
          pr.age <- t.count;
          pr.pending <- false))
      (Atomic.get t.pub_list);
    Mutex.unlock t.global_lock;
    let res = pr.result in
    pr.result <- None;
    res)
  else scan_combine_apply t pr
;;

let apply t request =
  let id = Domain.self () in
  let pr =
    try Hashtbl.find t.domain_records id with
    | Not_found ->
      let new_pr = init_pub_rec () in
      Hashtbl.add t.domain_records id new_pr;
      new_pr
  in
  pr.request <- request;
  pr.pending <- true;
  if not pr.active
  then (
    Atomic.set t.pub_list (pr :: Atomic.get t.pub_list);
    pr.active <- true);
  scan_combine_apply t pr
;;
