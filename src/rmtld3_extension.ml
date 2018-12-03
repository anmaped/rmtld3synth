(*
   RMTLD3 type extensions
 *)

open Sexplib
open Sexplib.Std
open Rmtld3

type kmap = KVar of var_id | KProp of prop | KForm of string
[@@deriving sexp]

type tm_var = Var of var_id [@@deriving sexp]

(*
	definition of the RMTLD in DNF
*)
type tm_disj =
  | Var of var_id
  | C of value
  | Dur of tm_disj * fm_disj
  | Plus of tm_disj * tm_disj
  | Times of tm_disj * tm_disj

and fm_atom =
  | Not of fm_atom
  | True
  | Prop of prop
  | Less of tm_disj * tm_disj
  | Equal of tm_disj * tm_disj
  | EqualD of tm_var * (tm_disj * fm_disj)
  | ULess of time * fm_disj * fm_disj
  | E of var_id * fm_disj

and fm_conj = [`And of fm_atom * fm_conj | `X of fm_atom]

and fm_disj = [`Or of fm_conj * fm_disj | `Conj of fm_conj]

(*
	intermediate representation for simplifying RMTLD formula in DNF; RD and RU constructors
*)
type tm_disj_ex =
  | Var of var_id
  | C of value
  | Dur of tm_disj_ex * fm_disj_ex
  | Plus of tm_disj_ex * tm_disj_ex
  | Times of tm_disj_ex * tm_disj_ex
  | RD of var_id

and fm_atom_ex =
  | Not of fm_atom_ex
  | True
  | Prop of prop
  | Less of tm_disj_ex * tm_disj_ex
  | Equal of tm_disj_ex * tm_disj_ex
  | EqualD of tm_var * (tm_disj_ex * fm_disj_ex)
  | ULess of time * fm_disj_ex * fm_disj_ex
  | E of var_id * fm_disj_ex
  | RU of prop

and fm_conj_ex = [`And of fm_atom_ex * fm_conj_ex | `X of fm_atom_ex]

and fm_disj_ex = [`Or of fm_conj_ex * fm_disj_ex | `Conj of fm_conj_ex]
[@@deriving sexp]

(*
	rigid formula: a formula containing only rigid terms
*)
type rg_tm =
  | Var of var_id
  | C of value
  | Plus of rg_tm * rg_tm
  | Times of rg_tm * rg_tm

and rg_atoms =
  | Not of rg_atoms
  | True
  | Prop of prop
  | Less of rg_tm * rg_tm
  | Equal of tm_var * rg_tm
  | ULess of time * fm_disj * fm_disj
  | E of fm_disj

and rg_fm_conj = [`And of rg_fm_conj * rg_atoms | `X of rg_atoms]

and rg_fm_disj = [`Or of rg_fm_conj * rg_fm_disj | `Conj of rg_fm_conj]

(*
	fm_disj_less is a formula composed by disjuntion of conjuctions of inequalities and constrained equalities
*)
type tm_disj_less =
  | Var of var_id
  | C of value
  | Dur of tm_disj_less * fm_disj_less
  | Plus of tm_disj_less * tm_disj_less
  | Times of tm_disj_less * tm_disj_less

and fm_conj_less_atom =
  | Not of fm_conj_less_atom
  | True
  | Less of tm_disj_less * tm_disj_less
  | Equal of tm_var * tm_disj_less

and fm_conj_less =
  [`And of fm_conj_less * fm_conj_less_atom | `X of fm_conj_less_atom]

and fm_disj_less = [`Or of fm_conj_less * fm_disj_less | `Conj of fm_conj_less]

(*
	fm_disj_notless is a formula without explicit polynomial inequalities
	- it only contains equalities of the form "Equal(Var(id), Dur(Var(id2), phi))"
*)
type tm_disj_notless = Var of var_id | C of value

and fm_atom_notless =
  | Not of fm_atom_notless
  | True
  | Prop of prop
  | EqualD of tm_var * (tm_disj_notless * fm_disj_notless)
  | ULess of time * fm_disj_notless * fm_disj_notless
  | E of var_id * fm_disj_notless

and fm_conj_notless =
  [`And of fm_atom_notless * fm_conj_notless | `X of fm_atom_notless]

and fm_disj_notless =
  [`Or of fm_conj_notless * fm_disj_notless | `Conj of fm_conj_notless]

(*
   Containers and helpers
 *)
module Fm_container = Map.Make (String)

(* RMTLD index type for fm_disj_ex type container *)
type idx_ct_fm_disj_ex =
  | KUntil of time * fm_disj_ex * fm_disj_ex
  | KDuration of fm_disj_ex * tm_disj_ex
  | KFormula of fm_disj_ex
[@@deriving sexp]

type map_of_fm = idx_ct Fm_container.t

type map_of_fm_disj_ex = idx_ct_fm_disj_ex Fm_container.t

let add_map key fm rmap = rmap := Fm_container.add key fm !rmap

let rem_map key rmap = Fm_container.remove key rmap

(*
 * *******************************
 * ** type conversion functions **
 * *******************************
*)

let rec tc_conj : 'a -> 'a -> 'a =
 fun xs ys ->
  match xs with `X x -> `And (x, ys) | `And (x, xs) -> `And (x, tc_conj xs ys)

let rec tc_disj : 'a -> 'a -> 'a =
 fun xs ys ->
  match xs with `Conj c -> `Or (c, ys) | `Or (x, xs) -> `Or (x, tc_disj xs ys)

(* From fm_disj to fm_disj_less *)
let fm_disj_to_fm_disj_less fm = fm

(* From fm_disj to fm_disj_ex *)
let rec tm_disj_to_tm_disj_ex (tm : tm_disj) : tm_disj_ex =
  match tm with
  | Var vid -> Var vid
  | C vl -> C vl
  | Dur (tm_d, fm_d) ->
      Dur (tm_disj_to_tm_disj_ex tm_d, fm_disj_to_fm_disj_ex fm_d)
  | Plus (tm1_d, tm2_d) ->
      Plus (tm_disj_to_tm_disj_ex tm1_d, tm_disj_to_tm_disj_ex tm2_d)
  | Times (tm1_d, tm2_d) ->
      Times (tm_disj_to_tm_disj_ex tm1_d, tm_disj_to_tm_disj_ex tm2_d)

and fm_atom_to_fm_atom_ex (atm : fm_atom) : fm_atom_ex =
  match atm with
  | Not atm -> Not (fm_atom_to_fm_atom_ex atm)
  | True -> True
  | Prop p -> Prop p
  | Less (tm1_d, tm2_d) ->
      Less (tm_disj_to_tm_disj_ex tm1_d, tm_disj_to_tm_disj_ex tm2_d)
  | Equal (tm1_d, tm2_d) ->
      Equal (tm_disj_to_tm_disj_ex tm1_d, tm_disj_to_tm_disj_ex tm2_d)
  | EqualD (tv, (tm_d, fm_d)) ->
      EqualD (tv, (tm_disj_to_tm_disj_ex tm_d, fm_disj_to_fm_disj_ex fm_d))
  | ULess (t, fm1_d, fm2_d) ->
      ULess (t, fm_disj_to_fm_disj_ex fm1_d, fm_disj_to_fm_disj_ex fm2_d)
  | E (vid, fm_d) -> E (vid, fm_disj_to_fm_disj_ex fm_d)

and fm_conj_to_fm_conj_ex (fm : fm_conj) : fm_conj_ex =
  match fm with
  | `And (atm, fm_c) ->
      `And (fm_atom_to_fm_atom_ex atm, fm_conj_to_fm_conj_ex fm_c)
  | `X atm -> `X (fm_atom_to_fm_atom_ex atm)

and fm_disj_to_fm_disj_ex (fm : fm_disj) : fm_disj_ex =
  match fm with
  | `Or (fm_c, fm_d) ->
      `Or (fm_conj_to_fm_conj_ex fm_c, fm_disj_to_fm_disj_ex fm_d)
  | `Conj fm_c -> `Conj (fm_conj_to_fm_conj_ex fm_c)

(*
   type conversion of fm_disj_ex to fm_disj 
*)
let rec fm_disj_of_fm_disj_ex (fm : fm_disj_ex) : fm_disj =
  match fm with
  | `Or (cnj, dnj) -> `Or (fm_conj_of_fm_conj_ex cnj, fm_disj_of_fm_disj_ex dnj)
  | `Conj cnj -> `Conj (fm_conj_of_fm_conj_ex cnj)

and fm_conj_of_fm_conj_ex (fm : fm_conj_ex) : fm_conj =
  match fm with
  | `And (at, cnj) -> `And (fm_atom_of_fm_atom_ex at, fm_conj_of_fm_conj_ex cnj)
  | `X at -> `X (fm_atom_of_fm_atom_ex at)

and fm_atom_of_fm_atom_ex (at : fm_atom_ex) : fm_atom =
  match at with
  | Not atm -> Not (fm_atom_of_fm_atom_ex atm)
  | True -> True
  | Prop p -> Prop p
  | Less (tm1_d, tm2_d) ->
      Less (tm_disj_of_tm_disj_ex tm1_d, tm_disj_of_tm_disj_ex tm2_d)
  | Equal (tm1_d, tm2_d) ->
      Equal (tm_disj_of_tm_disj_ex tm1_d, tm_disj_of_tm_disj_ex tm2_d)
  | EqualD (tv, (tm_d, fm_d)) ->
      EqualD (tv, (tm_disj_of_tm_disj_ex tm_d, fm_disj_of_fm_disj_ex fm_d))
  | ULess (t, fm1_d, fm2_d) ->
      ULess (t, fm_disj_of_fm_disj_ex fm1_d, fm_disj_of_fm_disj_ex fm2_d)
  | E (vid, fm_d) -> E (vid, fm_disj_of_fm_disj_ex fm_d)
  | _ -> raise (Failure "fm_atom_of_fm_atom_ex: conversion from RU error")

and tm_disj_of_tm_disj_ex (tm : tm_disj_ex) : tm_disj =
  match tm with
  | Var vid -> Var vid
  | C vl -> C vl
  | Dur (tm_d, fm_d) ->
      Dur (tm_disj_of_tm_disj_ex tm_d, fm_disj_of_fm_disj_ex fm_d)
  | Plus (tm1_d, tm2_d) ->
      Plus (tm_disj_of_tm_disj_ex tm1_d, tm_disj_of_tm_disj_ex tm2_d)
  | Times (tm1_d, tm2_d) ->
      Times (tm_disj_of_tm_disj_ex tm1_d, tm_disj_of_tm_disj_ex tm2_d)
  | _ -> raise (Failure "tm_disj_of_tm_disj_ex: conversion from RD error")

(*
   type conversion of fm_disj to fm_disj_notless
*)
let rec fm_disj_notless_of_fm_disj (fm : fm_disj) : fm_disj_notless =
  match fm with
  | `Or (cnj, dnj) ->
      `Or (fm_conj_notless_of_fm_conj cnj, fm_disj_notless_of_fm_disj dnj)
  | `Conj cnj -> `Conj (fm_conj_notless_of_fm_conj cnj)

and fm_conj_notless_of_fm_conj (fm : fm_conj) : fm_conj_notless =
  match fm with
  | `And (at, cnj) ->
      `And (fm_atom_notless_of_fm_atom at, fm_conj_notless_of_fm_conj cnj)
  | `X at -> `X (fm_atom_notless_of_fm_atom at)

and fm_atom_notless_of_fm_atom (at : fm_atom) : fm_atom_notless =
  match at with
  | Not atm -> Not (fm_atom_notless_of_fm_atom atm)
  | True -> True
  | Prop p -> Prop p
  | EqualD (tv, (tm_d, fm_d)) ->
      EqualD
        (tv, (tm_disj_notless_of_tm_disj tm_d, fm_disj_notless_of_fm_disj fm_d))
  | ULess (t, fm1_d, fm2_d) ->
      ULess
        (t, fm_disj_notless_of_fm_disj fm1_d, fm_disj_notless_of_fm_disj fm2_d)
  | E (vid, fm_d) -> E (vid, fm_disj_notless_of_fm_disj fm_d)
  | _ -> raise (Failure "fm_atom_notless_of_fm_atom: conversion error")

and tm_disj_notless_of_tm_disj (tm : tm_disj) : tm_disj_notless =
  match tm with
  | Var vid -> Var vid
  | C vl -> C vl
  | _ -> raise (Failure "tm_disj_notless_of_tm_disj: conversion error")

(*
   type conversion of fm_disj to fm
*)
let rec fm_of_fm_disj (fm : fm_disj) : fm =
  match fm with
  | `Or (cnj, dsj) -> Or (fm_of_fm_conj cnj, fm_of_fm_disj dsj)
  | `Conj cnj -> fm_of_fm_conj cnj

and fm_of_fm_conj (fm : fm_conj) : fm =
  match fm with
  | `And (at, cnj) -> mand (fm_of_fm_atom at) (fm_of_fm_conj cnj)
  | `X at -> fm_of_fm_atom at

and fm_of_fm_atom (fm : fm_atom) : fm =
  match fm with
  | Not at -> Not (fm_of_fm_atom at)
  | True -> True ()
  | Prop id -> Prop id
  | Less (tm1, tm2) -> LessThan (tm_of_tm_disj tm1, tm_of_tm_disj tm2)
  | Equal (tm1, tm2) -> equal (tm_of_tm_disj tm1) (tm_of_tm_disj tm2)
  | EqualD (tv, (tm, fm)) ->
      equal (tm_of_tm_disj_var tv)
        (Duration (tm_of_tm_disj tm, fm_of_fm_disj fm))
  | ULess (t, fm1, fm2) -> Until (t, fm_of_fm_disj fm1, fm_of_fm_disj fm2)
  | E (id, fm) -> Exists (id, fm_of_fm_disj fm)

and tm_of_tm_disj (tm : tm_disj) : tm =
  match tm with
  | Var id -> Variable id
  | C value -> Constant value
  | Dur (tm, fm) -> Duration (tm_of_tm_disj tm, fm_of_fm_disj fm)
  | Plus (tm1, tm2) -> FPlus (tm_of_tm_disj tm1, tm_of_tm_disj tm2)
  | Times (tm1, tm2) -> FTimes (tm_of_tm_disj tm1, tm_of_tm_disj tm2)

and tm_of_tm_disj_var (tmv : tm_var) : tm =
  match tmv with Var id -> Variable id

let tm_of_tm_disj_ex tm = tm_of_tm_disj (tm_disj_of_tm_disj_ex tm)

let fm_of_fm_atom_ex atm = fm_of_fm_atom (fm_atom_of_fm_atom_ex atm)

let fm_of_fm_conj_ex cnj = fm_of_fm_conj (fm_conj_of_fm_conj_ex cnj)

let fm_of_fm_disj_ex fm = fm_of_fm_disj (fm_disj_of_fm_disj_ex fm)

(*
  type conversion for lists of conjunctions and disjunctions
*)
let select lst =
  match lst with
  | x :: l -> (x, l)
  | _ -> raise (Failure "Selected an element in an empty list.")

let rec fm_atom_ex_lst_of_fm_conj_ex (fm : fm_conj_ex) : fm_atom_ex list =
  match fm with
  | `And (a, b) -> [a] @ fm_atom_ex_lst_of_fm_conj_ex b
  | `X a -> [a]

let rec fm_conj_ex_lst_of_fm_disj_ex (fm : fm_disj_ex) : fm_conj_ex list =
  match fm with
  | `Or (a, b) -> [a] @ fm_conj_ex_lst_of_fm_disj_ex b
  | `Conj a -> [a]

let fm_disj_ex_of_fm_conj_ex_lst (lst : fm_conj_ex list) : fm_disj_ex =
  match lst with
  | el :: x -> `Conj (List.fold_left (fun a b -> tc_conj a b) el x)
  | _ ->
      raise (Failure "fm_disj_ex_of_fm_conj_ex_lst from an empty/unary list")

let fm_conj_ex_of_fm_atom_ex_lst (lst : fm_atom_ex list) : fm_conj_ex =
  match lst with
  | el :: x -> List.fold_left (fun a b -> `And (b, a)) (`X el) x
  | _ ->
      raise (Failure "fm_conj_ex_of_fm_atom_ex_lst from an empty/unary list")

(*
   type conversion for maps composed by rmtld3 terms and formulas
 *)
let id_count = ref 1

let get_unique_id () =
  id_count := !id_count + 1 ;
  string_of_int !id_count

let rec tm_map_of_tm (tm : tm) (rmap : idx_ct Fm_container.t ref) : tm =
  match tm with
  | Constant value -> Constant value
  | Variable id -> Variable id
  | Duration (trm, sf) ->
      (* replace duration with one free variable *)
      let o_fm = fm_map_of_fm sf rmap in
      let o_tm = tm_map_of_tm trm rmap in
      let varid = "tm" ^ get_unique_id () in
      let tag : idx_ct = KDuration (o_fm, o_tm) in
      add_map varid tag rmap ; (* add formula and sub-term *)
                               Variable varid
  | FPlus (eta1, eta2) -> FPlus (tm_map_of_tm eta1 rmap, tm_map_of_tm eta2 rmap)
  | FTimes (eta1, eta2) ->
      FTimes (tm_map_of_tm eta1 rmap, tm_map_of_tm eta2 rmap)

and fm_map_of_fm (fm : fm) (rmap : idx_ct Fm_container.t ref) : fm =
  match fm with
  | True () -> True ()
  | Prop p -> Prop p
  | Not sf -> Not (fm_map_of_fm sf rmap)
  | Or (sf1, sf2) -> Or (fm_map_of_fm sf1 rmap, fm_map_of_fm sf2 rmap)
  | Until (pval, sf1, sf2) ->
      (* replace until operator with one proposition *)
      let o_fm1 = fm_map_of_fm sf1 rmap in
      let o_fm2 = fm_map_of_fm sf2 rmap in
      let varid = "fm" ^ get_unique_id () in
      let tag : idx_ct = KUntil (pval, o_fm1, o_fm2) in
      rmap := Fm_container.add varid tag !rmap ;
      Prop varid
  | Exists (var, sf) -> Exists (var, fm_map_of_fm sf rmap)
  | LessThan (tr1, tr2) ->
      LessThan (tm_map_of_tm tr1 rmap, tm_map_of_tm tr2 rmap)
  | a ->
      raise (Failure ("Unsupported term " ^ Sexp.to_string_hum (sexp_of_fm a)))

let rec tm_of_tm_map (tm : tm) (mapfm : idx_ct Fm_container.t) : tm =
  match tm with
  | Variable id -> (
    try
      (* is the variable id replaced ? *)
      match Fm_container.find id mapfm with
      | KDuration (sf, trm) -> Duration (trm, sf)
      | _ -> raise (Failure "tm_of_tm_map error: no KDuration")
    with Not_found -> Variable id )
  | _ -> raise (Failure ("bad unreplace: " ^ string_of_rmtld_tm tm))

and fm_of_fm_map (fm : fm) (mapfm : idx_ct Fm_container.t) : fm =
  (* replace identified free variables and temporal operators into the formula fm *)
  match fm with
  | True () -> True ()
  | Not
      (Or
        ( LessThan (Constant 1., Variable varid1)
        , LessThan (Variable varid2, Constant 1.) ))
    when varid1 = varid2 -> (
    (* get available sub-formula *)
    match Fm_container.find varid1 mapfm with
    | KUntil (pval, fm1, fm2) ->
        Until (pval, fm_of_fm_map fm1 mapfm, fm_of_fm_map fm2 mapfm)
    | _ -> raise (Failure "fm_of_fm_map error: no KUntil") )
  | LessThan (tm1, tm2) ->
      LessThan (tm_of_tm_map tm1 mapfm, tm_of_tm_map tm2 mapfm)
  | Prop p -> Prop p
  | Not sf -> Not (fm_of_fm_map sf mapfm)
  | Or (sf1, sf2) -> Or (fm_of_fm_map sf1 mapfm, fm_of_fm_map sf2 mapfm)
  | Exists (var, sf) -> Exists (var, fm_of_fm_map sf mapfm)
  | _ -> raise (Failure ("bad unreplace: " ^ string_of_rmtld_fm fm))

(*
   type conversion from map to tm and fm
*)
let rec fm_of_idx_ct_fm_disj_ex (fm_idx : idx_ct_fm_disj_ex)
    (mapfm : 'b Fm_container.t) : fm =
  match fm_idx with
  | KFormula fm -> fm_of_map_fm_disj_ex' fm mapfm
  | KUntil (v, fm1, fm2) ->
      Until
        (v, fm_of_map_fm_disj_ex' fm1 mapfm, fm_of_map_fm_disj_ex' fm2 mapfm)
  | _ -> raise (Failure "fm_of_idx_ct_fm_disj_ex is unmatched.")

and tm_of_map_fm_disj_ex (tm : tm_disj_ex) (mapfm : 'b Fm_container.t) : tm =
  match tm with Var id -> Variable id | a -> tm_of_tm_disj_ex a

(* CONTINUE HERE !! *)
and fm_of_map_fm_atom_ex (fm : fm_atom_ex) (mapfm : 'b Fm_container.t) : fm =
  match fm with
  | Prop p -> (
    try
      (* replace proposition with a formula or  an until operator *)
      fm_of_idx_ct_fm_disj_ex
        (Fm_container.find (Sexp.to_string (sexp_of_kmap (KForm p))) mapfm)
        mapfm
    with Not_found -> (
      try
        fm_of_idx_ct_fm_disj_ex
          (Fm_container.find (Sexp.to_string (sexp_of_kmap (KProp p))) mapfm)
          mapfm
      with Not_found -> Prop p ) )
  | Not atm -> Not (fm_of_map_fm_atom_ex atm mapfm)
  | a -> fm_of_fm_atom_ex a

and fm_of_map_fm_disj_ex'' (fm : fm_conj_ex) (mapfm : 'b Fm_container.t) : fm =
  match fm with
  | `And (atm, cnj) ->
      mand (fm_of_map_fm_atom_ex atm mapfm) (fm_of_map_fm_disj_ex'' cnj mapfm)
  | `X atm -> fm_of_map_fm_atom_ex atm mapfm

and fm_of_map_fm_disj_ex' (fm : fm_disj_ex) (mapfm : 'b Fm_container.t) : fm =
  match fm with
  | `Conj fm_c -> fm_of_map_fm_disj_ex'' fm_c mapfm
  | `Or (fm1_c, fm2_d) ->
      Or (fm_of_map_fm_disj_ex'' fm1_c mapfm, fm_of_map_fm_disj_ex' fm2_d mapfm)

let fm_of_map_fm_disj_ex mapfm : fm =
  let id = "mainfm" in
  let omapfm = rem_map id mapfm in
  (* map set without 'mainfm' *)
  
  (* get formula with id 'mainfm' *)
  try
    fm_of_idx_ct_fm_disj_ex
      (Fm_container.find (Sexp.to_string (sexp_of_kmap (KForm id))) mapfm)
      omapfm
  with Not_found ->
    raise (Failure "fm_of_map_fm_disj_ex: mainfm is not available.")

(*
 * helpers for map containers
*)
let string_of_fm_map fm_map =
  let stringify : 'a -> idx_ct -> 'a -> 'a =
   fun ky value str ->
    match value with
    | KUntil (pval, fm1, fm2) ->
        str ^ ky ^ " -> Until " ^ string_of_float pval ^ " "
        ^ string_of_rmtld_fm fm1 ^ " " ^ string_of_rmtld_fm fm2 ^ "\n"
    | KDuration (fm, tm) ->
        str ^ ky ^ " -> Duration " ^ string_of_rmtld_fm fm ^ " "
        ^ string_of_rmtld_tm tm ^ "\n"
    | KFormula fm -> str ^ ky ^ " -> Formula " ^ string_of_rmtld_fm fm ^ "\n"
  in
  Fm_container.fold stringify fm_map ""

let string_of_fm_disj_ex_map fm_disj_ex_map =
  let stringify : 'a -> idx_ct_fm_disj_ex -> 'a -> 'a =
   fun ky value str ->
    match value with
    | KUntil (pval, fm1, fm2) ->
        str ^ ky ^ " -> Until " ^ string_of_float pval ^ " "
        ^ Sexp.to_string (sexp_of_fm_disj_ex fm1)
        ^ " "
        ^ Sexp.to_string (sexp_of_fm_disj_ex fm2)
        ^ "\n"
    | KDuration (fm, tm) ->
        str ^ ky ^ " -> Duration "
        ^ Sexp.to_string (sexp_of_fm_disj_ex fm)
        ^ " "
        ^ Sexp.to_string (sexp_of_tm_disj_ex tm)
        ^ "\n"
    | KFormula fm ->
        str ^ ky ^ " -> Formula "
        ^ Sexp.to_string (sexp_of_fm_disj_ex fm)
        ^ "\n"
  in
  Fm_container.fold stringify fm_disj_ex_map ""

let print_fm_map fm_map = print_endline (string_of_fm_map fm_map)

let print_fm_map_ex fm_map_ex =
  print_endline (string_of_fm_disj_ex_map fm_map_ex)
