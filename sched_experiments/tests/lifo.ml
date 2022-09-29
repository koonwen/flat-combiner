open Seq_con_util
open Schedulr

let () = Instance.LIFO.init num_domains ~f:testing_function |> ignore