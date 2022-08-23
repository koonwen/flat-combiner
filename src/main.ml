let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()
;;

(* Command line interface *)

open Cmdliner

let setup_log = Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())
let main prog = Cmd.(v (info "flat combiner") Term.(const prog $ setup_log))

module Fcq = Lib.Fc_queue.FC_queue (Int)

let fcq = Fcq.create ()

let prog () =
  let _ = Fcq.enqueue fcq 5 in
  let _ = Fcq.enqueue fcq 6 in
  let _ = Fcq.dequeue fcq in
  Fcq.traverse_publist fcq;
  Fcq.log_queue fcq
;;

(* let res1 = Fcq.dequeue fcq in
let res2 = Fcq.dequeue fcq in
Printf.printf "%d -> %d \n" res1 res2 *)

let () =
  match Cmd.eval (main prog) with
  | _ -> exit (if Logs.err_count () > 0 then 1 else 0)
;;