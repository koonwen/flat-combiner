let d_spawner f ~num_domains = Array.init num_domains (fun _ -> Domain.spawn f)
let pp_int_int_seq = Fmt.(seq ~sep:semi (pair ~sep:comma int int))
let pp_int_int_list = Fmt.(list ~sep:semi int)

(* Add ppx tests for this *)
let check_unique seq =
  let module IntS = Set.Make (Int) in
  let s = ref IntS.empty in
  let rec aux = function
    | Seq.Nil -> true
    | Seq.Cons ((_, hd), t) ->
      if IntS.mem hd !s
      then false
      else (
        s := IntS.add hd !s;
        aux (t ()))
  in
  aux (seq ())
;;

let check_elements n num_domains seq =
  let div = n in
  let tbl = Hashtbl.create n in
  let rec aux = function
    | Seq.Nil ->
      Hashtbl.fold
        (fun _ v acc ->
          if v = num_domains
          then acc
          else failwith "[check_elements_2] elements inconsistent")
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

let distribute num_domains seq =
  let tbl = Hashtbl.create num_domains in
  let rec aux = function
    | Seq.Nil -> tbl
    | Seq.Cons ((id, hd), t) ->
      (match Hashtbl.find_opt tbl id with
       | Some v -> Hashtbl.replace tbl id (hd :: v)
       | None -> Hashtbl.add tbl id [ hd ]);
      aux (t ())
  in
  aux (seq ())
;;

let check_order num_domains seq =
  let tbl = Hashtbl.create num_domains in
  let rec aux = function
    | Seq.Nil ->
      (* assert (Hashtbl.length tbl = num_domains); *)
      true
    | Seq.Cons ((id, hd), t) ->
      (match Hashtbl.find_opt tbl id with
       | Some v ->
         if v >= hd
         then (
           Printf.printf "Expected v >= hd but got (%d, %d)\n" v hd;
           failwith "[check_order] order not sequential")
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
