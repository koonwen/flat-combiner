type ('ds, 'res) pub_rec = {
  mutable active : bool;
  mutable request : 'ds -> 'res;
  mutable result : 'res Option.t;
  mutable age : int
}

type ('ds, 'res) t = {
  global_lock : Mutex.t ; mutable count : int;
  mutable ds : 'ds;
  pub_list : ('ds, 'res) pub_rec List.t Atomic.t;
  domain_records : (('ds, 'res) pub_rec) Domain.DLS.key
}

let init_pub_rec () = {
  active = false;
  request = (fun () -> None);
  result = None;
  age = 0
}

let init ~data_structure:ds ~num_domains = {
  global_lock = Mutex.create ();
  count = 0;
  pub_list = Atomic.make [];
  ds;
  domain_records = Domain.DLS.new_key (fun () -> init_pub_rec ())
}

let rec scan_combine_apply t pr =
  match pr.result with
  | Some result -> (* if result has been computed, we're done *) result
  | None ->
    (* if not yet computed, then spin  *)
    if not @@ Mutex.try_lock t.global_lock
    then scan_combine_apply t pr
    else begin
      (* otherwise, we're the split combiner *)
      t.count <- t.count + 1;
      (* complete all the requests *)
      List.iter
        (fun pr ->
           match pr.result with
           | None ->
             pr.result <- Some (pr.request t.ds);
             pr.age <- t.count;
           | _ -> ())
        (Atomic.get t.pub_list);
      (* done *)
      Mutex.unlock t.global_lock;
      (* extract our own domain's own result *)
      let res = pr.result in
      pr.result <- None;         (* is this needed? *)
      Option.get res
    end

let apply t request =
  (* retrieve the PR for the current domain *)
  let pr = Domain.DLS.get t.domain_records in
  (* write the request to the PR for the domain  *)
  pr.request <- request;
  pr.result <- None;
  (* if not active - i.e not in pub list, then add to pub list and set as active *)
  if not pr.active
  then (
    while not @@ let ls = Atomic.get t.pub_list in Atomic.compare_and_set t.pub_list (pr :: ls) ls do () done;
    pr.active <- true);
  (* retrieve result *)
  scan_combine_apply t pr

