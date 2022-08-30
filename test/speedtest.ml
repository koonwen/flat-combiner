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

let time ~name ~repeat f f' =
  let t0 = Benchmark.make 0L in
  for _ = 1 to Float.to_int repeat do
    f ();
    f' ()
  done;
  let b = Benchmark.sub (Benchmark.make 0L) t0 in
  Printf.printf
    "(%s): %s --- Average : %f\n"
    name
    (Benchmark.to_string b)
    (b.wall /. repeat)
;;

let () =
  let repeat = 5. in
  let n = 10_000_000 in
  Printf.printf "Recommended Domain count for machine = %d\n" num_domains;
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
    Queues.Lock_queue.clearer
;;
