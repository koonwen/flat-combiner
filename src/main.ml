(* let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()
;; *)

(* Command line interface *)

(* open Cmdliner

let setup_log = Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())
let main prog = Cmd.(v (info "flat combiner") Term.(const prog $ setup_log)) *)

(* let prog () =
  let open Fcq_thread in
  let fcq = FC.create_queue () in
  let t1 = Thread.create enqueuer fcq in
  let t2 = Thread.create dequeuer fcq in
  Thread.join t1;
  Thread.join t2
;;

let () = prog () *)

(* let main prog = Cmd.(v (info "flat combiner") Term.(const prog $ setup_log))

let () =
  match Cmd.eval (main prog) with
  | _ -> exit (if Logs.err_count () > 0 then 1 else 0)
;; *)