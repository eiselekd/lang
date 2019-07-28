
type filename = string
type pos = (filename * int * int)
type span = {lo: pos; hi: pos}

(*
 * Auxiliary hashtable functions.
 *)

let htab_keys (htab:('a,'b) Hashtbl.t) : ('a list) =
  Hashtbl.fold (fun k _ accum -> k :: accum) htab []
;;

let sorted_htab_keys (tab:('a, 'b) Hashtbl.t) : 'a array =
  let keys = Array.of_list (htab_keys tab) in
    Array.sort compare keys;
    keys
;;

let htab_vals (htab:('a,'b) Hashtbl.t) : ('b list)  =
  Hashtbl.fold (fun _ v accum -> v :: accum) htab []
;;

let htab_pairs (htab:('a,'b) Hashtbl.t) : (('a * 'b) list) =
  Hashtbl.fold (fun k v accum -> (k,v) :: accum) htab []
;;

let htab_search (htab:('a,'b) Hashtbl.t) (k:'a) : ('b option) =
  if Hashtbl.mem htab k
  then Some (Hashtbl.find htab k)
  else None
;;

let htab_search_or_default
    (htab:('a,'b) Hashtbl.t)
    (k:'a)
    (def:unit -> 'b)
    : 'b =
  match htab_search htab k with
      Some v -> v
    | None -> def()
;;

let htab_search_or_add
    (htab:('a,'b) Hashtbl.t)
    (k:'a)
    (mk:unit -> 'b)
    : 'b =
  let def () =
    let v = mk() in
      Hashtbl.add htab k v;
      v
  in
    htab_search_or_default htab k def
;;

let htab_put (htab:('a,'b) Hashtbl.t) (a:'a) (b:'b) : unit =
  assert (not (Hashtbl.mem htab a));
  Hashtbl.add htab a b
;;

let htab_map
    (htab:('a,'b) Hashtbl.t)
    (f:'a -> 'b -> ('c * 'd))
    : (('c,'d) Hashtbl.t) =
  let ntab = Hashtbl.create (Hashtbl.length htab) in
  let g a b =
    let (c,d) = f a b in
      htab_put ntab c d
  in
    Hashtbl.iter g htab;
    ntab
;;


let htab_fold
    (fn:'a -> 'b -> 'c -> 'c)
    (init:'c)
    (h:('a, 'b) Hashtbl.t) : 'c =
  let accum = ref init in
  let f a b = accum := (fn a b (!accum)) in
    Hashtbl.iter f h;
    !accum
;;


let reduce_hash_to_list
    (fn:'a -> 'b -> 'c)
    (h:('a, 'b) Hashtbl.t)
    : ('c list) =
  htab_fold (fun a b ls -> (fn a b) :: ls) [] h
;;

(*
 * Local Variables:
 * fill-column: 78;
 * indent-tabs-mode: nil
 * buffer-file-coding-system: utf-8-unix
 * compile-command: "make -k -C . 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 *)
