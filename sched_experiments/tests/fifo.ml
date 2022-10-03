open Seq_con_util
open Schedulr

let () = Instance.FIFO.init num_domains ~f:testing_function |> ignore
