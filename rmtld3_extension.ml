
(*
   RMTLD3 type extensions
 *)

open Rmtld3

type tm_var = Var of var_id

(*
	RMTLD index type for fm formula container
*)
type idx_ct = KUntil of time * formula * formula | KDuration of formula * term | KFormula of formula

(*
	definition of the RMTLD in DNF
*)
type tm_disj = Var of var_id | C of value | Dur of tm_disj * fm_disj | Plus of tm_disj * tm_disj | Times of tm_disj * tm_disj
and atoms = Not of atoms | True | Prop of prop | Less of tm_disj * tm_disj | Equal of tm_disj * tm_disj | EqualD of tm_var * (tm_disj * fm_disj) | ULess of time * fm_disj * fm_disj | E of var_id * fm_disj 
and fm_conj = [`And of atoms * fm_conj | `X of atoms]
and fm_disj = [`Or of fm_conj * fm_disj  | `Conj of fm_conj]


(*
	intermediate representation for simplifying RMTLD formula in DNF; RD and RU constructors
*)
type tm_disj_ex = Var of var_id | C of value | Dur of tm_disj_ex * fm_disj_ex | Plus of tm_disj_ex * tm_disj_ex | Times of tm_disj_ex * tm_disj_ex | RD of var_id
and fm_atom_ex = Not of fm_atom_ex | True | Prop of prop | Less of tm_disj_ex * tm_disj_ex | Equal of tm_disj_ex * tm_disj_ex | EqualD of tm_var * (tm_disj_ex * fm_disj_ex) | ULess of time * fm_disj_ex * fm_disj_ex | E of var_id * fm_disj_ex | RU of prop
and fm_conj_ex = [`And of fm_atom_ex * fm_conj_ex |  `X of fm_atom_ex]
and fm_disj_ex = [`Or of fm_conj_ex * fm_disj_ex | `Conj of fm_conj_ex]

type idx_ct_fm_disj_ex = KUntil of time * fm_disj_ex * fm_disj_ex | KDuration of fm_disj_ex * tm_disj_ex | KFormula of fm_disj_ex


(*
	rigid formula: a formula containing only rigid terms
*)
type rg_tm = Var of var_id | C of value | Plus of rg_tm * rg_tm | Times of rg_tm * rg_tm
and rg_atoms = Not of rg_atoms | True | Prop of prop | Less of rg_tm * rg_tm | Equal of tm_var * rg_tm | ULess of time * fm_disj * fm_disj | E of fm_disj 
and rg_fm_conj = [`And of rg_fm_conj * rg_atoms | `X of rg_atoms]
and rg_fm_disj = [`Or of rg_fm_conj * rg_fm_disj | `Conj of rg_fm_conj]


(*
	fm_disj_less is a formula composed by disjuntion of conjuctions of inequalities and constrained equalities
*)
type tm_disj_less = Var of var_id | C of value | Dur of tm_disj_less * fm_disj_less | Plus of tm_disj_less * tm_disj_less | Times of tm_disj_less * tm_disj_less
and fm_conj_less_atom =  Not of fm_conj_less_atom | True | Less of tm_disj_less * tm_disj_less | Equal of tm_var * tm_disj_less
and fm_conj_less = [`And of fm_conj_less * fm_conj_less_atom | `X of fm_conj_less_atom]
and fm_disj_less = [`Or of fm_conj_less * fm_disj_less | `Conj of fm_conj_less]

(*
	fm_disj_notless is a formula without explicit polynomial inequalities
	- it only contains equalities of the form "Equal(Var(id), Dur(Var(id2), phi))"
*)
type tm_notless = Dur of tm_var * fm_disj_notless
and fm_atom_notless = Not of fm_atom_notless | True | Prop of prop | Equal of tm_var * tm_notless | ULess of time * fm_disj * fm_disj | E of fm_disj
and fm_conj_notless = [`And of fm_conj_notless * fm_atom_notless | `X of fm_atom_notless]
and fm_disj_notless = [`Or of fm_conj_notless * fm_disj_notless | `Conj of fm_conj_notless]


(*
   Containers and helpers
 *)
module Fm_container = Map.Make(String);;
let add_map key fm rmap = rmap := Fm_container.add key fm !rmap
let rem_map key rmap = Fm_container.remove key rmap


(*
	type conversion functions
*)

let rec tc_conj : 'a -> 'a -> 'a = fun xs ys ->
  match xs with
  | `X x -> `And (x, ys)
  | `And (x, xs) -> `And (x, tc_conj xs ys)

let rec tc_disj : 'a -> 'a -> 'a = fun xs ys -> match xs with
  | `Conj c -> `Or (c, ys)
  | `Or (x, xs) -> `Or (x, tc_disj xs ys)


(* From fm_disj to fm_disj_less *)
let fm_disj_to_fm_disj_less fm =
	fm


(* From fm_disj to fm_disj_notless *)
let fm_disj_to_fm_disj_notless (fm: fm_disj) : fm_disj_notless  = 
	match fm with
	_ -> `Conj(`X(Prop("a"))) 


(* From fm_disj to fm_disj_ex *)
let rec tm_disj_to_tm_disj_ex (tm: tm_disj) : tm_disj_ex =
  match tm with
  | Var(vid)                   -> Var(vid)
  | C(vl)                      -> C(vl)
  | Dur(tm_d,fm_d)             -> Dur(tm_disj_to_tm_disj_ex tm_d, fm_disj_to_fm_disj_ex fm_d)
  | Plus(tm1_d,tm2_d)          -> Plus(tm_disj_to_tm_disj_ex tm1_d, tm_disj_to_tm_disj_ex tm2_d)
  | Times(tm1_d,tm2_d)         -> Times(tm_disj_to_tm_disj_ex tm1_d, tm_disj_to_tm_disj_ex tm2_d)

and atoms_to_fm_atom_ex (atm: atoms) : fm_atom_ex =
  match atm with
  | Not(atm)                 -> Not(atoms_to_fm_atom_ex atm)
  | True                     -> True
  | Prop(p)                  -> Prop(p)
  | Less(tm1_d,tm2_d)        -> Less(tm_disj_to_tm_disj_ex tm1_d, tm_disj_to_tm_disj_ex tm2_d)
  | Equal(tm1_d,tm2_d)       -> Equal(tm_disj_to_tm_disj_ex tm1_d, tm_disj_to_tm_disj_ex tm2_d)
  | EqualD(tv,(tm_d,fm_d))   -> EqualD(tv,(tm_disj_to_tm_disj_ex tm_d, fm_disj_to_fm_disj_ex fm_d))
  | ULess(t,fm1_d,fm2_d)     -> ULess(t,fm_disj_to_fm_disj_ex fm1_d, fm_disj_to_fm_disj_ex fm2_d)
  | E(vid,fm_d)              -> E(vid,fm_disj_to_fm_disj_ex fm_d)

and fm_conj_to_fm_conj_ex (fm: fm_conj) : fm_conj_ex =
  match fm with
  | `And(atm,fm_c) -> `And(atoms_to_fm_atom_ex atm, fm_conj_to_fm_conj_ex fm_c)
  | `X(atm)        -> `X(atoms_to_fm_atom_ex atm)

and fm_disj_to_fm_disj_ex (fm: fm_disj) : fm_disj_ex =
  match fm with
  | `Or(fm_c,fm_d) -> `Or(fm_conj_to_fm_conj_ex fm_c, fm_disj_to_fm_disj_ex fm_d)
  | `Conj(fm_c)    -> `Conj(fm_conj_to_fm_conj_ex fm_c)


(*
   type conversion of fm_disj_ex to fm_disj 
*)
let fm_disj_of_fm_disj_ex (fm: fm_disj_ex) : fm_disj =
	(* TODO *)
	`Conj(`X(True))

let fm_atom_notless_of_fm_atom_ex (fm: fm_atom_ex) : fm_atom_notless =
  (* TODO *)
  True

let fm_conj_notless_of_fm_conj_ex (fm: fm_conj_ex) : fm_conj_notless =
  (* TODO *)
  `X(True)


(*
   type conversion of fm_disj to fm
*)
let fm_of_fm_disj (fm: fm_disj) : fm =
  (* TODO *)
  True()


(*
  type conversion for lists of conjunctions and disjunctions
*)
let select lst = (List.hd lst, List.tl lst)

let rec fm_atom_ex_lst_of_fm_conj_ex (fm: fm_conj_ex) : fm_atom_ex list =
  match fm with
  | `And(a,b) -> [a] @ (fm_atom_ex_lst_of_fm_conj_ex b)
  | `X(a) -> [a]

let rec fm_conj_ex_lst_of_fm_disj_ex (fm: fm_disj_ex) : fm_conj_ex list =
  match fm with
  | `Or(a,b) -> [a] @ (fm_conj_ex_lst_of_fm_disj_ex b)
  | `Conj(a) -> [a]

let fm_disj_ex_of_fm_conj_ex_lst (lst: fm_conj_ex list) : fm_disj_ex =
  match lst with
  | el::x -> `Conj(List.fold_left (fun a b -> tc_conj a b) el x)
  | _ -> raise (Failure ("fm_disj_ex_of_fm_conj_ex_lst from an empty/unary list"))

let fm_conj_ex_of_fm_atom_ex_lst (lst: fm_atom_ex list) : fm_conj_ex =
  match lst with
  | el::x -> List.fold_left (fun a b -> `And(b, a)) (`X(el)) x
  | _ -> raise (Failure ("fm_conj_ex_of_fm_atom_ex_lst from an empty/unary list"))

