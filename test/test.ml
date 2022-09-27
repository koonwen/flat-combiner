open Fcq_effects

let () =
  enqueuer 0 10;
  let res = dequeuer 10 in
  List.iter (fun (_, v) -> Printf.printf "(_, %d) " v) res
;;
