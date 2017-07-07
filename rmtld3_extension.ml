(*pp camlp4o `ocamlfind query type_conv`/pa_type_conv.cma  `ocamlfind query pa_sexp_conv`/pa_sexp_conv.cma  -I `ocamlfind query sexplib` -I `ocamlfind query pa_sexp_conv` *)

(*
   RMTLD3 type extensions
 *)

open Rmtld3

type tm_var = Var of var_id with sexp

(*
	RMTLD index type for fm formula container
*)
type idx_ct = KUntil of time * formula * formula | KDuration of formula * term | KFormula of formula

(*
	definition of the RMTLD in DNF
*)
type tm_disj = Var of var_id | C of value | Dur of tm_disj * fm_disj | Plus of tm_disj * tm_disj | Times of tm_disj * tm_disj
and fm_atom = Not of fm_atom | True | Prop of prop | Less of tm_disj * tm_disj | Equal of tm_disj * tm_disj | EqualD of tm_var * (tm_disj * fm_disj) | ULess of time * fm_disj * fm_disj | E of var_id * fm_disj 
and fm_conj = [`And of fm_atom * fm_conj | `X of fm_atom]
and fm_disj = [`Or of fm_conj * fm_disj  | `Conj of fm_conj]


(*
	intermediate representation for simplifying RMTLD formula in DNF; RD and RU constructors
*)
type tm_disj_ex = Var of var_id | C of value | Dur of tm_disj_ex * fm_disj_ex | Plus of tm_disj_ex * tm_disj_ex | Times of tm_disj_ex * tm_disj_ex | RD of var_id
and fm_atom_ex = Not of fm_atom_ex | True | Prop of prop | Less of tm_disj_ex * tm_disj_ex | Equal of tm_disj_ex * tm_disj_ex | EqualD of tm_var * (tm_disj_ex * fm_disj_ex) | ULess of time * fm_disj_ex * fm_disj_ex | E of var_id * fm_disj_ex | RU of prop
and fm_conj_ex = [`And of fm_atom_ex * fm_conj_ex |  `X of fm_atom_ex]
and fm_disj_ex = [`Or of fm_conj_ex * fm_disj_ex | `Conj of fm_conj_ex]
with sexp

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
type tm_disj_notless = Var of var_id | C of value 
and fm_atom_notless = Not of fm_atom_notless | True | Prop of prop | EqualD of tm_var * (tm_disj_notless * fm_disj_notless) | ULess of time * fm_disj_notless * fm_disj_notless | E of var_id * fm_disj_notless
and fm_conj_notless = [`And of fm_atom_notless * fm_conj_notless | `X of fm_atom_notless]
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


(* From fm_disj to fm_disj_ex *)
let rec tm_disj_to_tm_disj_ex (tm: tm_disj) : tm_disj_ex =
  match tm with
  | Var(vid)                   -> Var(vid)
  | C(vl)                      -> C(vl)
  | Dur(tm_d,fm_d)             -> Dur(tm_disj_to_tm_disj_ex tm_d, fm_disj_to_fm_disj_ex fm_d)
  | Plus(tm1_d,tm2_d)          -> Plus(tm_disj_to_tm_disj_ex tm1_d, tm_disj_to_tm_disj_ex tm2_d)
  | Times(tm1_d,tm2_d)         -> Times(tm_disj_to_tm_disj_ex tm1_d, tm_disj_to_tm_disj_ex tm2_d)
and fm_atom_to_fm_atom_ex (atm: fm_atom) : fm_atom_ex =
  match atm with
  | Not(atm)                 -> Not(fm_atom_to_fm_atom_ex atm)
  | True                     -> True
  | Prop(p)                  -> Prop(p)
  | Less(tm1_d,tm2_d)        -> Less(tm_disj_to_tm_disj_ex tm1_d, tm_disj_to_tm_disj_ex tm2_d)
  | Equal(tm1_d,tm2_d)       -> Equal(tm_disj_to_tm_disj_ex tm1_d, tm_disj_to_tm_disj_ex tm2_d)
  | EqualD(tv,(tm_d,fm_d))   -> EqualD(tv,(tm_disj_to_tm_disj_ex tm_d, fm_disj_to_fm_disj_ex fm_d))
  | ULess(t,fm1_d,fm2_d)     -> ULess(t,fm_disj_to_fm_disj_ex fm1_d, fm_disj_to_fm_disj_ex fm2_d)
  | E(vid,fm_d)              -> E(vid,fm_disj_to_fm_disj_ex fm_d)
and fm_conj_to_fm_conj_ex (fm: fm_conj) : fm_conj_ex =
  match fm with
  | `And(atm,fm_c) -> `And(fm_atom_to_fm_atom_ex atm, fm_conj_to_fm_conj_ex fm_c)
  | `X(atm)        -> `X(fm_atom_to_fm_atom_ex atm)
and fm_disj_to_fm_disj_ex (fm: fm_disj) : fm_disj_ex =
  match fm with
  | `Or(fm_c,fm_d) -> `Or(fm_conj_to_fm_conj_ex fm_c, fm_disj_to_fm_disj_ex fm_d)
  | `Conj(fm_c)    -> `Conj(fm_conj_to_fm_conj_ex fm_c)


(*
   type conversion of fm_disj_ex to fm_disj 
*)
let rec fm_disj_of_fm_disj_ex (fm: fm_disj_ex) : fm_disj =
  match fm with
  | `Or(cnj,dnj) -> `Or(fm_conj_of_fm_conj_ex cnj,fm_disj_of_fm_disj_ex dnj) 
  | `Conj(cnj)   -> `Conj(fm_conj_of_fm_conj_ex cnj)
and fm_conj_of_fm_conj_ex (fm: fm_conj_ex) : fm_conj =
  match fm with
  | `And(at,cnj) -> `And(fm_atom_of_fm_atom_ex at, fm_conj_of_fm_conj_ex cnj)
  | `X(at)       -> `X(fm_atom_of_fm_atom_ex at)
and fm_atom_of_fm_atom_ex (at: fm_atom_ex) : fm_atom =
  match at with
  | Not(atm)                 -> Not(fm_atom_of_fm_atom_ex atm)
  | True                     -> True
  | Prop(p)                  -> Prop(p)
  | Less(tm1_d,tm2_d)        -> Less(tm_disj_of_tm_disj_ex tm1_d, tm_disj_of_tm_disj_ex tm2_d)
  | Equal(tm1_d,tm2_d)       -> Equal(tm_disj_of_tm_disj_ex tm1_d, tm_disj_of_tm_disj_ex tm2_d)
  | EqualD(tv,(tm_d,fm_d))   -> EqualD(tv,(tm_disj_of_tm_disj_ex tm_d, fm_disj_of_fm_disj_ex fm_d))
  | ULess(t,fm1_d,fm2_d)     -> ULess(t,fm_disj_of_fm_disj_ex fm1_d, fm_disj_of_fm_disj_ex fm2_d)
  | E(vid,fm_d)              -> E(vid,fm_disj_of_fm_disj_ex fm_d)
  | _                        -> raise (Failure ("fm_atom_of_fm_atom_ex: conversion from RU error"))
and tm_disj_of_tm_disj_ex (tm: tm_disj_ex) : tm_disj =
  match tm with
  | Var(vid)                   -> Var(vid)
  | C(vl)                      -> C(vl)
  | Dur(tm_d,fm_d)             -> Dur(tm_disj_of_tm_disj_ex tm_d, fm_disj_of_fm_disj_ex fm_d)
  | Plus(tm1_d,tm2_d)          -> Plus(tm_disj_of_tm_disj_ex tm1_d, tm_disj_of_tm_disj_ex tm2_d)
  | Times(tm1_d,tm2_d)         -> Times(tm_disj_of_tm_disj_ex tm1_d, tm_disj_of_tm_disj_ex tm2_d)
  | _                          -> raise (Failure ("tm_disj_of_tm_disj_ex: conversion from RD error"))

(*
   type conversion of fm_disj to fm_disj_notless
*)
let rec fm_disj_notless_of_fm_disj (fm: fm_disj) : fm_disj_notless =
  match fm with
  | `Or(cnj,dnj) -> `Or(fm_conj_notless_of_fm_conj cnj,fm_disj_notless_of_fm_disj dnj) 
  | `Conj(cnj)   -> `Conj(fm_conj_notless_of_fm_conj cnj)
and fm_conj_notless_of_fm_conj (fm: fm_conj) : fm_conj_notless =
  match fm with
  | `And(at,cnj) -> `And(fm_atom_notless_of_fm_atom at, fm_conj_notless_of_fm_conj cnj)
  | `X(at)       -> `X(fm_atom_notless_of_fm_atom at)
and fm_atom_notless_of_fm_atom (at: fm_atom) : fm_atom_notless =
  match at with
  | Not(atm)                 -> Not(fm_atom_notless_of_fm_atom atm)
  | True                     -> True
  | Prop(p)                  -> Prop(p)
  | EqualD(tv,(tm_d,fm_d))   -> EqualD(tv,(tm_disj_notless_of_tm_disj tm_d, fm_disj_notless_of_fm_disj fm_d))
  | ULess(t,fm1_d,fm2_d)     -> ULess(t,fm_disj_notless_of_fm_disj fm1_d, fm_disj_notless_of_fm_disj fm2_d)
  | E(vid,fm_d)              -> E(vid,fm_disj_notless_of_fm_disj fm_d)
  | _                        -> raise (Failure ("fm_atom_notless_of_fm_atom: conversion error"))
and tm_disj_notless_of_tm_disj (tm: tm_disj) : tm_disj_notless =
  match tm with
  | Var(vid)                   -> Var(vid)
  | C(vl)                      -> C(vl)
  | _                          -> raise (Failure ("tm_disj_notless_of_tm_disj: conversion error"))


(*
   type conversion of fm_disj to fm
*)
let rec fm_of_fm_disj (fm: fm_disj) : fm =
  match fm with
  | `Or(cnj,dsj)       -> Or(fm_of_fm_conj cnj, fm_of_fm_disj dsj)
  | `Conj(cnj)         -> fm_of_fm_conj cnj
and fm_of_fm_conj (fm: fm_conj) : fm =
  match fm with
  | `And(at,cnj)       -> mand (fm_of_fm_atom at) (fm_of_fm_conj cnj)
  | `X(at)             -> fm_of_fm_atom at
and fm_of_fm_atom (fm: fm_atom) : fm =
  match fm with
  | Not(at)            -> Not(fm_of_fm_atom at)
  | True               -> True()
  | Prop(id)           -> Prop(id)
  | Less(tm1,tm2)      -> LessThan(fm_of_tm_disj tm1, fm_of_tm_disj tm2)
  | Equal(tm1,tm2)     -> equal (fm_of_tm_disj tm1) (fm_of_tm_disj tm2)
  | EqualD(tv,(tm,fm)) -> equal (fm_of_tm_disj_var tv) (Duration(fm_of_tm_disj tm, fm_of_fm_disj fm))
  | ULess(t,fm1,fm2)   -> Until(t,fm_of_fm_disj fm1, fm_of_fm_disj fm2)
  | E(id,fm)           -> Exists(id,fm_of_fm_disj fm)
and fm_of_tm_disj (tm: tm_disj) : tm =
  match tm with
  | Var(id)            -> Variable(id)
  | C(value)           -> Constant(value)
  | Dur(tm,fm)         -> Duration(fm_of_tm_disj tm,fm_of_fm_disj fm)
  | Plus(tm1,tm2)      -> FPlus(fm_of_tm_disj tm1,fm_of_tm_disj tm2)
  | Times(tm1,tm2)     -> FTimes(fm_of_tm_disj tm1,fm_of_tm_disj tm2)
and fm_of_tm_disj_var (tmv: tm_var) : tm =
  match tmv with
  | Var(id) -> Variable(id)



(*
  type conversion for lists of conjunctions and disjunctions
*)
let select lst =
  match lst with
    x::l -> (x, l)
  | _ -> raise (Failure ("Selected an element in an empty list."))

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

