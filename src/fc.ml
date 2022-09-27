let get_domain_id () = (Domain.self () :> int)

type elt = int

let pp_elt = Fmt.int

type op_thunk =
  | Enq of (unit -> unit)
  | Deq of (unit -> elt)
  | Nop
[@@deriving show]

(* Is this a good place to use GADTs? *)
type op_res =
  | REnq of unit
  | RDeq of elt
  | Nil
[@@deriving show]

module PubRecord = struct
  type t =
    { mutable op_thunk : op_thunk
    ; mutable result : op_res
    ; mutable age : int
    ; mutable active : bool
    ; id : int
    }
  [@@deriving show]

  let key =
    (* Initializer so that Domain Local Publication Record can be created on get command *)
    Domain.DLS.new_key (fun _ ->
      { op_thunk = Nop; result = Nil; age = 0; active = false; id = get_domain_id () })
  ;;

  let self () = Domain.DLS.get key

  let update_thunk ~op_thunk =
    let self = self () in
    self.op_thunk <- op_thunk
  ;;

  let update_result ~result =
    let self = self () in
    self.result <- result;
    self.age <- self.age + 1
  ;;

  let res_avail t =
    match t.result with
    | Nil -> false
    | _ -> true
  ;;
end

type pub_list = PubRecord.t list [@@deriving show]

type t =
  { queue : elt Queue.t
  ; lock : Mutex.t
  ; pub_list : pub_list Atomic.t
  ; mutable count : int
  }

let create () =
  { queue = Queue.create ()
  ; lock = Mutex.create ()
  ; pub_list = Atomic.make []
  ; count = 0
  }
;;

let rec publish t pr =
  (* Fmt.pr "Domain %a Publishing to List" Fmt.int (get_domain_id ()); *)
  let old_list = Atomic.get t.pub_list in
  if Atomic.compare_and_set t.pub_list old_list (pr :: old_list)
  then pr.active <- true
  else publish t pr
;;

let exec (pr : PubRecord.t) =
  (* Fmt.pr
    "Domain %a Executing record %s"
    Fmt.int
    (Domain.self () :> int)
    (PubRecord.show pr); *)
  match pr.op_thunk with
  | Enq f ->
    let result = REnq (f ()) in
    pr.result <- result;
    pr.op_thunk <- Nop
  | Deq f ->
    let result = RDeq (f ()) in
    pr.result <- result;
    pr.op_thunk <- Nop
  | Nop -> ()
;;

let rec scan_combine_apply t (r : PubRecord.t) =
  if PubRecord.res_avail r (* Return result and reset record *)
  then (
    let res = r.result in
    r.result <- Nil;
    res)
  else if (* If you hold the lock, you are the combiner *)
          Mutex.try_lock t.lock
  then (
    (* Fmt.pr "Domain %a Scan_combine_apply%!\n" Fmt.int (get_domain_id ()); *)
    let pub_list = Atomic.get t.pub_list in
    (* Fmt.pr "[Domain %a] Before:\n" Fmt.int (get_domain_id ());
    Fmt.pr "Pub_list %s\n\n" (show_pub_list pub_list); *)
    List.iter exec pub_list;
    (* Fmt.pr "Pub_list %s\n\n" (show_pub_list pub_list); *)
    Mutex.unlock t.lock;
    scan_combine_apply t r)
  else (
    (* Spin and check periodically if the lock has been freed*)
    let cnt = ref 0 in
    while
      (* Spin and wait for result to be installed *)
      (not @@ PubRecord.res_avail r) && !cnt <= 1000
    do
      cnt := !cnt + 1;
      (* Domain.cpu_relax ()  *)
      Fmt.pr "Domain %a spinning\n%!" Fmt.int (get_domain_id ())
    done;
    scan_combine_apply t r)
;;

let enq t v =
  (* Fmt.pr
    "[Domain %a] enq: curr Q length = %d\n"
    Fmt.int
    (get_domain_id ())
    (t.queue |> Queue.length); *)
  (* Check if record is active *)
  let pr = PubRecord.self () in
  if not pr.active then publish t pr;
  (* Create thunk and install into record *)
  let op_thunk = Enq (fun () -> Queue.push v t.queue) in
  PubRecord.update_thunk ~op_thunk;
  let op_res = scan_combine_apply t pr in
  match op_res with
  | REnq _ -> ()
  | c ->
    let e =
      Printf.sprintf "[Fc.enq]: Return value contructor mismatch got %s" (show_op_res c)
    in
    failwith e
;;

let deq t =
  (* Check if record is active *)
  let pr = PubRecord.self () in
  if not pr.active then publish t pr;
  (* Create thunk and install into record *)
  let op_thunk = Deq (fun () -> Queue.take t.queue) in
  PubRecord.update_thunk ~op_thunk;
  let op_res = scan_combine_apply t pr in
  match op_res with
  | RDeq v -> v
  | _ -> failwith "[Fc.enq]: Return value contructor mismatch"
;;

(* ===================================================================== *)
let pp_int_seq = Fmt.(seq ~sep:comma int)

module IntS = Set.Make (Int)

let enquer fc lo hi =
  for i = lo to hi do
    enq fc i
  done
;;

let test_enq fc n =
  let set = IntS.add_seq (fc.queue |> Queue.to_seq) IntS.empty in
  Printf.printf "%d | %d\n" (Queue.length fc.queue) (IntS.cardinal set);
  (* Fmt.pr "Enqueued: %a\n" pp_int_seq (Queue.to_seq fc.queue); *)
  assert (IntS.cardinal set == n);
  assert (IntS.min_elt set == 1);
  assert (IntS.max_elt set == n)
;;

let dequer fc n =
  let acc = ref Seq.empty in
  for _ = 1 to n do
    acc := Seq.cons (deq fc) !acc
  done;
  acc
;;

let test_deq acc n =
  let set = IntS.add_seq !acc IntS.empty in
  assert (IntS.cardinal set != n);
  assert (IntS.min_elt set != 1);
  assert (IntS.max_elt set != n)
;;

let test_multiple_domains fc n =
  let num_domains = 2 in
  let chunk = n / num_domains in
  let d1 = Domain.spawn (fun _ -> enquer fc 1 chunk) in
  let d2 = Domain.spawn (fun _ -> enquer fc (chunk + 1) n) in
  List.iter Domain.join [ d1; d2 ]
;;

module T = Domainslib.Task

let async_enquer pool fc lo hi =
  T.parallel_for ~start:lo ~finish:hi ~body:(fun i -> enq fc i) pool
;;

let dlib_main () =
  let pool = T.setup_pool ~num_domains:(Domain.recommended_domain_count - 1) () in
  let fc = create () in
  let n = 1_000_000 in
  let t1 = Benchmark.make 0L in
  T.run pool (fun _ -> async_enquer pool fc 1 n);
  let res1 = Benchmark.sub (Benchmark.make 0L) t1 in
  let q = Queue.create () in
  let t2 = Benchmark.make 0L in
  for i = 1 to n do
    Queue.push i q
  done;
  let res2 = Benchmark.sub (Benchmark.make 0L) t2 in
  Printf.printf "(async): %s\n" (Benchmark.to_string res1);
  Printf.printf "(sync): %s" (Benchmark.to_string res2);
  test_enq fc n;
  T.teardown_pool pool
;;

let () = dlib_main ()
(* Printexc.record_backtrace true;
  try main () with
  | e ->
    Printexc.to_string e |> print_endline;
    Printexc.print_backtrace stdout *)
