open Benchmark

let num_domains = Domain.recommended_domain_count

let test_enq_sequential_consistency f n =
  let divisions = n / num_domains in
  let dom_l = ref [] in
  for _ = 1 to num_domains do
    dom_l := Domain.spawn (fun () -> f 1 divisions) :: !dom_l
  done;
  List.iter Domain.join !dom_l
;;

let time ~name ~repeat f reset =
  let b_list = ref [] in
  for _ = 1 to Float.to_int repeat do
    let t0 = Benchmark.make 0L in
    f ();
    b_list := Benchmark.sub (Benchmark.make 0L) t0 :: !b_list;
    reset ()
  done;
  let collated = List.fold_left Benchmark.add (List.hd !b_list) (List.tl !b_list) in
  Printf.printf
    "(%s): %s --- Average : %f\n"
    name
    (Benchmark.to_string collated)
    (collated.wall /. repeat)
;;

let () =
  let repeat = 5. in
  let n = 10_000_000 in
  Printf.printf "Running (%d) Domains for the tests\n" num_domains;
  print_endline "Benchmark results:";
  time
    ~name:"fcq_threads_enq"
    ~repeat
    (fun () -> test_enq_sequential_consistency Fcq_threads.enqueuer_v3 n)
    Fcq_threads.FC_Queue.clear;
  time
    ~name:"fcq_domains_enq"
    ~repeat
    (fun () -> test_enq_sequential_consistency Fcq_domains.enqueuer n)
    Fcq_domains.FC_Queue.clear;
  time
    ~name:"coarse_lock_enq"
    ~repeat
    (fun () -> test_enq_sequential_consistency Queues.Lock_queue.enqueuer n)
    Queues.Lock_queue.clearer;
  time
    ~name:"mpmc_enq"
    ~repeat
    (fun () -> test_enq_sequential_consistency Queues.Mpmc_queue.enqueuer n)
    Queues.Mpmc_queue.recreate;
  time
    ~name:"lockfree_enq"
    ~repeat
    (fun () -> test_enq_sequential_consistency Lockfree_queue.enqueuer n)
    Lockfree_queue.recreate
;;

(* Comments
   - How does order of execution change the results? GC and the Heap allocations?
   - Not sure why this fails on MacOS but works on Fedora
   - Potential inconsistencies with the tests accounting for differences:
      - The underlying implementation of both the mpmc queue & lockfree queue uses an array whereas the Stdlib uses a linked list.
*)