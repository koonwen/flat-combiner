open Schedulr

module T = struct
  let num_threads = 10
  let pool = Array.make num_threads (Promise.empty ())
end

module FC = Fcq.Make (T)

let testing_function () =
  let n = 1_000_000 in
  for i = 0 to T.num_threads - 1 do
    T.pool.(i) <- Scheduler.schedule (fun _ -> FC.enqueuer ~id:i 1 n)
  done;
  for i = 0 to T.num_threads - 1 do
    Scheduler.await T.pool.(i)
  done;
  exit 0
;;

let () = Instance.FIFO.init 7 ~f:testing_function |> ignore