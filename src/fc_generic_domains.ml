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
  ; domain_records : (Domain.id, 'a publication_record) Hashtbl.t
  }

let init_pub_rec () =
  { active = false; request = (fun () -> None); result = None; pending = true; age = 0 }
;;

let create ~data_structure:ds ~num_domains =
  { global_lock = Mutex.create ()
  ; count = 0
  ; pub_list = []
  ; ds
  ; domain_records = Hashtbl.create num_domains
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
  let id = Domain.self () in
  let pr =
    try Hashtbl.find t.domain_records id with
    | Not_found ->
      let new_pr = init_pub_rec () in
      Hashtbl.add t.domain_records id new_pr;
      new_pr
  in
  if not pr.active
  then (
    t.pub_list <- pr :: t.pub_list;
    pr.active <- true);
  pr.pending <- true;
  pr.result <- None;
  pr.request <- request;
  scan_combine_apply t pr
;;
