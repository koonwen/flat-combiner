open Schedulr
open Fcq

let n = 1_000_000
let num_domains = 7

module T = struct
  let num_threads = 10
  let pool = Array.make num_threads (Promise.empty ())
end

module FC = Fcq.Make (T)

let test_enq_sequential_consistency n num_threads =
  (* let tbl = Util.distribute num_threads (FC._q |> Queue.to_seq) in
  Hashtbl.iter (fun key v -> Fmt.pr "(Thread %d)\n\n%a\n\n" key pp_int_list v) tbl;
  Fmt.(pr "%a\n" pp_int_int_queue FC._q); *)
  (* Check length of Queue *)
  assert (FC._q |> Queue.length = n * num_threads);
  (* Check that elements are unique *)
  assert (FC._q |> Queue.to_seq |> Util.check_elements n num_threads);
  (* Check sequential consistency *)
  assert (FC._q |> Queue.to_seq |> Util.check_order num_threads)
;;

let testing_function () =
  for i = 0 to T.num_threads - 1 do
    T.pool.(i) <- Scheduler.schedule (fun _ -> FC.enqueuer ~id:i 1 n)
  done;
  for i = 0 to T.num_threads - 1 do
    Scheduler.await T.pool.(i)
  done;
  test_enq_sequential_consistency n T.num_threads;
  exit 0
;;