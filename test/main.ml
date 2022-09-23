let short_tests = 100_000
let long_tests = 10_000_000

let fc_threads_queue_tests =
  let open Test_fcq_threads in
  Alcotest.(
    ( "Flat-combiner Threads Implementation"
    , [ test_case
          "enq sequential consistency slow"
          `Slow
          (test_fcq_threads_enq long_tests)
      ; test_case
          "deq sequential consistency slow"
          `Slow
          (test_fcq_threads_deq long_tests)
      ; test_case
          "enq sequential consistency quick"
          `Quick
          (test_fcq_threads_enq short_tests)
      ; test_case
          "deq sequential consistency quick"
          `Quick
          (test_fcq_threads_deq short_tests)
      ] ))
;;

let fc_domains_queue_tests =
  let open Test_fcq_domains in
  Alcotest.(
    ( "Flat-combiner Domains Implementation"
    , [ test_case
          "enq sequential consistency slow"
          `Slow
          (test_fcq_domains_enq long_tests)
      ; test_case
          "deq sequential consistency slow"
          `Slow
          (test_fcq_domains_deq long_tests)
      ; test_case
          "enq sequential consistency quick"
          `Quick
          (test_fcq_domains_enq short_tests)
      ; test_case
          "deq sequential consistency quick"
          `Quick
          (test_fcq_domains_deq short_tests)
      ] ))
;;

let fc_domains_queue_tests =
  let open Test_fcq_effects in
  Alcotest.(
    ( "Flat-combiner Effects Implementation"
    , [ test_case
          "enq sequential consistency slow"
          `Slow
          (test_fcq_effects_enq long_tests)
      ; test_case
          "deq sequential consistency slow"
          `Slow
          (test_fcq_effects_deq long_tests)
      ; test_case
          "enq sequential consistency quick"
          `Quick
          (test_fcq_effects_enq short_tests)
      ; test_case
          "deq sequential consistency quick"
          `Quick
          (test_fcq_effects_deq short_tests)
      ] ))
;;

let coarse_lock_q_tests =
  let open Test_lock_queue in
  Alcotest.(
    ( "Coarse Lock Queue Implementation"
    , [ test_case "enq sequential consistency slow" `Slow (test_lock_queue_enq long_tests)
      ; test_case "deq sequential consistency slow" `Slow (test_lock_queue_deq long_tests)
      ; test_case
          "enq sequential consistency quick"
          `Quick
          (test_lock_queue_enq short_tests)
      ; test_case
          "deq sequential consistency quick"
          `Quick
          (test_lock_queue_deq short_tests)
      ] ))
;;

let mpmc_queue_tests =
  let open Test_mpmc_queue in
  Alcotest.(
    ( "MPMC Queue Implementation"
    , [ test_case
          "enq sequential consistency slow "
          `Slow
          (test_mpmc_queue_enq long_tests)
      ; test_case
          "deq sequential consistency slow "
          `Slow
          (test_mpmc_queue_deq long_tests)
      ; test_case
          "enq sequential consistency quick"
          `Quick
          (test_mpmc_queue_enq short_tests)
      ; test_case
          "deq sequential consistency quick"
          `Quick
          (test_mpmc_queue_deq short_tests)
      ] ))
;;

(* Run it *)
let () =
  let open Alcotest in
  run
    "Sequential Consistency Tests"
    [ fc_threads_queue_tests
    ; fc_domains_queue_tests
    ; coarse_lock_q_tests
    ; mpmc_queue_tests
    ]
;;
