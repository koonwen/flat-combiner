let check_elements lo hi seq =
  let module IntSet = Set.Make (Int) in
  let rec aux set = function
    | Seq.Nil -> true
    | Seq.Cons (hd, t) ->
      if lo <= hd && hd <= hi && not (IntSet.mem hd set)
      then aux (IntSet.add hd set) (t ())
      else false
  in
  aux IntSet.empty (seq ())
;;

let check_order n seq =
  let l, r = 1, n / 2 in
  let rec aux l r = function
    | Seq.Nil -> true
    | Seq.Cons (hd, t) ->
      if hd >= l && hd >= r
      then aux l hd (t ())
      else if hd >= l
      then aux hd r (t ())
      else false
  in
  aux l r (seq ())
;;

let rec check_descending = function
  | [] -> true
  | [ _ ] -> true
  | h1 :: h2 :: t -> if h1 >= h2 then check_descending (h2 :: t) else false
;;

let rec populate lo hi q =
  if lo <= hi
  then (
    Queue.add lo q;
    populate (lo + 1) hi q)
;;
