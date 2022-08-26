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
  ; thread_records : 'a publication_record Array.t
  }

let create ~data_structure:ds ~num_threads =
  let init_pub_rec =
    { active = false; request = (fun () -> None); result = None; pending = true; age = 0 }
  in
  { global_lock = Mutex.create ()
  ; count = 0
  ; pub_list = []
  ; ds
  ; thread_records = Array.make (num_threads + 1) init_pub_rec
  }
;;

let rec scan_combine_apply t pr =
  if not pr.pending
  then pr.result
  else if Mutex.try_lock t.global_lock
  then (
    List.iter
      (fun pr ->
        pr.result <- pr.request ();
        pr.age <- pr.age + 1;
        pr.pending <- false)
      t.pub_list;
    Mutex.unlock t.global_lock;
    pr.result)
  else scan_combine_apply t pr
;;

let apply t request =
  let pr = t.thread_records.(Thread.id (Thread.self ())) in
  if not pr.active
  then (
    t.pub_list <- pr :: t.pub_list;
    pr.active <- true);
  pr.pending <- true;
  pr.result <- None;
  pr.request <- request;
  scan_combine_apply t pr
;;
