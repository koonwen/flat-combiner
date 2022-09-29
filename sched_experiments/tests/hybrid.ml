open Seq_con_util
open Schedulr

let () = Instance.Hybrid_alternating.init num_domains ~f:testing_function |> ignore
