module Fcq = Lib.Fc_queue.FC_queue (Int)

let fcq = Fcq.create ()

let () =
  let _ = Fcq.enqueue fcq 5 in
  let _ = Fcq.enqueue fcq 6 in
  let _ = Fcq.dequeue fcq in
  Fcq.traverse_publist fcq
;;

(* let res1 = Fcq.dequeue fcq in
  let res2 = Fcq.dequeue fcq in
  Printf.printf "%d -> %d \n" res1 res2 *)
