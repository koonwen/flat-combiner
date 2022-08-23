let hello () = Logs.app (fun m -> m "Hello horrible world!")

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()
;;

(* Command line interface *)

open Cmdliner

let setup_log = Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())
let main () = Cmd.(v (info "flat combiner") Term.(const hello $ setup_log))

let () =
  match Cmd.eval (main ()) with
  | _ -> exit (if Logs.err_count () > 0 then 1 else 0)
;;
