(* open Core
open Core_bench

let seq_Q = Queue.create ()

let rec iter_push n =
  if n > 0
  then (
    Queue.enqueue seq_Q n;
    iter_push (n - 1))
;;

let rec iter_pop n =
  if n > 0
  then (
    let _ = Queue.dequeue_exn seq_Q in
    iter_pop (n - 1))
;;

let test () =
  iter_push 1_000_000;
  iter_pop 1_000_000
;;

let _ =
  Command_unix.run
    (Bench.make_command [ Bench.Test.create ~name:"Sequential Queue" test ])
;; *)
