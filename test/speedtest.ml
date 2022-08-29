open Benchmark

let num_domains = Domain.recommended_domain_count

let test_enq_sequential_consistency f n =
  let divisions = n / num_domains in
  let dom_l = ref [] in
  for _ = 0 to num_domains do
    dom_l := Domain.spawn (fun () -> f 1 divisions) :: !dom_l
  done;
  List.iter Domain.join !dom_l
;;

let time ~name ~iter f =
  let t0 = Benchmark.make 0L in
  for _ = 1 to Float.to_int iter do
    f ()
  done;
  let b = Benchmark.sub (Benchmark.make 0L) t0 in
  Printf.printf "(%s): %s --- Average : %f\n" name (Benchmark.to_string b) (b.wall /. iter)
;;

(* let res =
  latencyN
    5L
    [ "fcq_domains", Test_fcq_domains.test_deq_sequential_consistency, 1_000_000
    ; "fcq_threads", Test_fcq_threads.test_deq_sequential_consistency, 1_000_000
    ]
;; *)

let () =
  Printf.printf "Recommended Domain count for machine == %d\n" num_domains;
  print_endline "Benchmark results:";
  time ~name:"fcq_domains_enq" ~iter:1. (fun () ->
    test_enq_sequential_consistency Fcq_domains.enqueuer 10_000_000);
  time ~name:"coarse_lock_deq" ~iter:1. (fun () ->
    test_enq_sequential_consistency Test_lock_queue.enqueuer 10_000_000)
;;
