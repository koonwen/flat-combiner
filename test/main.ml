(* Build with `ocamlbuild -pkg alcotest simple.byte` *)

(* The tests *)
let test_fcq_threads_enq n () =
  Alcotest.(check unit) "Passed" () (Test_fcq_threads.test_enq_sequential_consistency n)
;;

let test_fcq_threads_deq n () =
  Alcotest.(check unit) "Passed" () (Test_fcq_threads.test_deq_sequential_consistency n)
;;

let test_fcq_domains_enq n () =
  Alcotest.(check unit) "Passed" () (Test_fcq_domains.test_enq_sequential_consistency n)
;;

let test_fcq_domains_deq n () =
  Alcotest.(check unit) "Passed" () (Test_fcq_domains.test_deq_sequential_consistency n)
;;

let test_lock_queue_enq n () =
  Alcotest.(check unit) "Passed" () (Test_lock_queue.test_enq_sequential_consistency n)
;;

let test_lock_queue_deq n () =
  Alcotest.(check unit) "Passed" () (Test_lock_queue.test_deq_sequential_consistency n)
;;

(* Run it *)
let () =
  let open Alcotest in
  run
    "FC Tests"
    [ ( "Flat-combiner Threads Implementation"
      , [ test_case "enq sequential consistency" `Slow (test_fcq_threads_enq 10_000_000)
        ; test_case "deq sequential consistency" `Slow (test_fcq_threads_deq 10_000_000)
        ; test_case "enq sequential consistency" `Quick (test_fcq_threads_enq 100_000)
        ; test_case "deq sequential consistency" `Quick (test_fcq_threads_deq 100_000)
        ] )
    ; ( "Flat-combiner Domains Implementation"
      , [ test_case "enq sequential consistency" `Slow (test_fcq_domains_enq 10_000_000)
        ; test_case "deq sequential consistency" `Slow (test_fcq_domains_deq 10_000_000)
        ; test_case "enq sequential consistency" `Quick (test_fcq_domains_enq 100_000)
        ; test_case "deq sequential consistency" `Quick (test_fcq_domains_deq 100_000)
        ] )
    ; ( "Coarse Lock Queue Implementation"
      , [ test_case "enq sequential consistency" `Slow (test_lock_queue_enq 10_000_000)
        ; test_case "deq sequential consistency" `Slow (test_lock_queue_deq 10_000_000)
        ; test_case "enq sequential consistency" `Quick (test_lock_queue_enq 100_000)
        ; test_case "deq sequential consistency" `Quick (test_lock_queue_deq 100_000)
        ] )
    ]
;;
