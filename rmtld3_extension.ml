
(* RMTLD extension types for proof trees *)

open Rmtld3

type tm_var = Var of var_id

(*
	definition of the RMTLD in DNF form
*)
type tm = Var of var_id | C of value | Dur of tm * fm_disj | Plus of tm * tm | Times of tm * tm
and atoms = Prop of prop | Less of tm * tm | Equal of tm_var * tm | ULess of fm_disj * fm_disj | E of fm_disj 
and fm_conj = And of fm_conj * atoms | Not of atoms
and fm_disj = Or of fm_conj * fm_disj | Conj of fm_conj

(*
	rigid formula
*)
type rg_tm = Var of var_id | C of value | Plus of rg_tm * rg_tm | Times of rg_tm * rg_tm
and rg_atoms = Prop of prop | Less of rg_tm * rg_tm | Equal of tm_var * rg_tm | ULess of fm_disj * fm_disj | E of fm_disj 
and rg_fm_conj = And of rg_fm_conj * rg_atoms | Not of rg_atoms
and rg_fm_disj = Or of rg_fm_conj * rg_fm_disj | Conj of rg_fm_conj


(*
	fm_disj_less is a formula composed by disjuntion of conjuctions of inequalities 
*)
type tm_disj_less = Var of var_id | C of value | Dur of tm_disj_less * fm_disj_less | Plus of tm_disj_less * tm_disj_less | Times of tm_disj_less * tm_disj_less
and fm_conj_less_atom = Less of tm_disj_less * tm_disj_less | Equal of tm_var * tm_disj_less
and fm_conj_less = And of fm_conj_less * fm_conj_less_atom | Not of fm_conj_less_atom
and fm_disj_less = Or of fm_conj_less * fm_disj_less | Conj of fm_conj_less

(*
	fm_disj_notless is a formula without explicit polynomial inequalities
	- it only contains equalities of the form "Equal(Var(id), Dur(Var(id2), phi))"
*)
type tm_dur = Dur of tm_var * fm_disj_notless
and fm_conj_notless_atom = Prop of prop | Equal of tm_var * tm_dur | ULess of fm_disj * fm_disj | E of fm_disj 
and fm_conj_notless = And of fm_conj_notless * fm_conj_notless_atom | Not of fm_conj_notless_atom
and fm_disj_notless = Or of fm_conj_notless * fm_disj_notless | Conj of fm_conj_notless


(*
	conversion steps
*)

(* From fm_disj to fm_disj_less *)


(* From fm_disj to fm_disj_notless *)
let fm_disj_to_fm_disj_notless fm = 
	fm


