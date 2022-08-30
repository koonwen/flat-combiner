open Lockfree.Ws_deque.M

let _q = ref (create ())

let recreate () =
  let new_q = create () in
  _q := new_q
;;

let rec enqueuer lo hi =
  if lo <= hi
  then (
    let id = Domain.self () in
    push !_q (id, lo);
    enqueuer (lo + 1) hi)
  else ()
;;

let dequeuer n =
  let acc = ref [] in
  let rec aux n =
    if n > 0
    then (
      try
        let v = pop !_q in
        acc := v :: !acc;
        aux (n - 1)
      with
      | e -> aux (n - 1))
    else ()
  in
  aux n;
  !acc
;;
