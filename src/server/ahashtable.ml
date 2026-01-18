
(** Atomic operations for thread-safe hash table access. *)

type ('k, 'v) table = ('k, 'v) Hashtbl.t Atomic.t


let add_atomic tbl_atomic k v =
  let rec loop () =
    let old = Atomic.get tbl_atomic in
    let new_tbl = Hashtbl.copy old in
    Hashtbl.replace new_tbl k v ;
    if not (Atomic.compare_and_set tbl_atomic old new_tbl) then loop ()
  in
  loop ()

let filter_map_inplace_atomic f tbl_atomic =
  let rec loop () =
    let old_tbl = Atomic.get tbl_atomic in
    let new_tbl = Hashtbl.copy old_tbl in
    Hashtbl.filter_map_inplace f new_tbl ;
    if not (Atomic.compare_and_set tbl_atomic old_tbl new_tbl) then loop ()
  in
  loop ()

let remove_atomic tbl_atomic key =
  let rec loop () =
    let old_tbl = Atomic.get tbl_atomic in
    if not (Hashtbl.mem old_tbl key) then false
    else
      let new_tbl = Hashtbl.copy old_tbl in
      Hashtbl.remove new_tbl key ;
      if Atomic.compare_and_set tbl_atomic old_tbl new_tbl then true
      else loop ()
  in
  let _ = loop () in
  ()

let replace_atomic tbl_atomic key value =
  let rec loop () =
    let old_tbl = Atomic.get tbl_atomic in
    let new_tbl = Hashtbl.copy old_tbl in
    Hashtbl.replace new_tbl key value ;
    if not (Atomic.compare_and_set tbl_atomic old_tbl new_tbl) then loop ()
  in
  loop ()

let fold_atomic f tbl_atomic acc =
  let tbl = Atomic.get tbl_atomic in
  Hashtbl.fold f tbl acc

let length_atomic tbl_atomic = Hashtbl.length (Atomic.get tbl_atomic)

let find_opt_atomic tbl_atomic k = Hashtbl.find_opt (Atomic.get tbl_atomic) k
