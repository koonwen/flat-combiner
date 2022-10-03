let rec fib n = if n = 0 then 0 else if n = 1 then 1 else fib (n - 1) + fib (n - 2)

let rec fib_para n =
  if n < 20
  then fib n
  else if n = 0
  then 0
  else if n = 1
  then 1
  else (
    let t1 = Schedulr.Scheduler.schedule (fun _ -> fib_para (n - 1)) in
    let t2 = Schedulr.Scheduler.schedule (fun _ -> fib_para (n - 2)) in
    Schedulr.Scheduler.await t1 + Schedulr.Scheduler.await t2)
;;

let expensive_comp () = fib 45
let expensive_para_comp () = fib_para 45

let () =
  (* expensive_comp () |> Printf.printf "%d\n%!"; *)
  Schedulr.Instance.LIFO.(
    init 7 ~f:(fun _ ->
      Printf.printf "%d%!\n" (expensive_para_comp ());
      Stdlib.exit 0)
    |> ignore)
;;
