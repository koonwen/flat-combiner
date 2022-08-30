let d_spawner f ~num_domains = Array.init num_domains (fun _ -> Domain.spawn f)

(* Add ppx tests for this *)
let check_elements n num_domains seq =
  let div = n / num_domains in
  let tbl = Hashtbl.create n in
  let rec aux = function
    | Seq.Nil ->
      Hashtbl.fold
        (fun _ v acc ->
          if v = num_domains
          then acc
          else failwith "[check_elements] elements inconsistent")
        tbl
        true
    | Seq.Cons ((_, hd), t) ->
      assert (1 <= hd && hd <= div);
      if Hashtbl.mem tbl hd
      then (
        let binding = Hashtbl.find tbl hd in
        assert (binding <= num_domains);
        Hashtbl.replace tbl hd (binding + 1))
      else Hashtbl.add tbl hd 1;
      aux (t ())
  in
  aux (seq ())
;;

let check_order num_domains seq =
  let tbl = Hashtbl.create num_domains in
  let rec aux = function
    | Seq.Nil ->
      assert (Hashtbl.length tbl = num_domains);
      true
    | Seq.Cons ((id, hd), t) ->
      (match Hashtbl.find_opt tbl id with
       | Some v ->
         if v + 1 != hd
         then failwith "[check_order] order not sequential"
         else Hashtbl.replace tbl id hd
       | None -> Hashtbl.add tbl id hd);
      aux (t ())
  in
  aux (seq ())
;;

let rec check_descending = function
  | [] -> true
  | [ _ ] -> true
  | h1 :: h2 :: t -> if h1 >= h2 then check_descending (h2 :: t) else false
;;

let rec populate id lo hi q =
  if lo <= hi
  then (
    Queue.add (id, lo) q;
    populate id (lo + 1) hi q)
;;
