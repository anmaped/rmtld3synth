
(* RMTLD extension types for proof trees *)

open Rmtld3

type tm_var = Var of var_id

(*
	definition of the RMTLD in DNF
*)
type tm_disj = Var of var_id | C of value | Dur of tm_disj * fm_disj | Plus of tm_disj * tm_disj | Times of tm_disj * tm_disj
and atoms = True | Prop of prop | Less of tm_disj * tm_disj | Equal of tm_disj * tm_disj | EqualD of tm_var * (tm_disj * fm_disj) | ULess of time * fm_disj * fm_disj | E of var_id * fm_disj 
and fm_conj = And of fm_conj * atoms | Not of atoms | Atom of atoms
and fm_disj = Or of fm_disj * fm_conj | Conj of fm_conj

(*
	intermediate representation for simplifying RMTLD formula in DNF; Ex_tm and Ex_fm constructors
*)
type tm_ex = Var of var_id | C of value | Dur of tm_ex * fm_disj_ex | Plus of tm_ex * tm_ex | Times of tm_ex * tm_ex | Ex_tm of var_id
and atoms_ex = True | Prop of prop | Less of tm_ex * tm_ex | Equal of tm_var * tm_ex | ULess of fm_disj_ex * fm_disj_ex | E of fm_disj_ex | Ex_fm of prop
and fm_conj_ex = And of fm_conj_ex * atoms_ex | Not of atoms_ex
and fm_disj_ex = Or of fm_conj_ex * fm_disj_ex | Conj of fm_conj_ex

(*
	rigid formula: a formula containing only rigid terms
*)
type rg_tm = Var of var_id | C of value | Plus of rg_tm * rg_tm | Times of rg_tm * rg_tm
and rg_atoms = True | Prop of prop | Less of rg_tm * rg_tm | Equal of tm_var * rg_tm | ULess of fm_disj * fm_disj | E of fm_disj 
and rg_fm_conj = And of rg_fm_conj * rg_atoms | Not of rg_atoms
and rg_fm_disj = Or of rg_fm_conj * rg_fm_disj | Conj of rg_fm_conj


(*
	fm_disj_less is a formula composed by disjuntion of conjuctions of inequalities and constrained equalities
*)
type tm_disj_less = Var of var_id | C of value | Dur of tm_disj_less * fm_disj_less | Plus of tm_disj_less * tm_disj_less | Times of tm_disj_less * tm_disj_less
and fm_conj_less_atom = True | Less of tm_disj_less * tm_disj_less | Equal of tm_var * tm_disj_less
and fm_conj_less = And of fm_conj_less * fm_conj_less_atom | Not of fm_conj_less_atom
and fm_disj_less = Or of fm_conj_less * fm_disj_less | Conj of fm_conj_less

(*
	fm_disj_notless is a formula without explicit polynomial inequalities
	- it only contains equalities of the form "Equal(Var(id), Dur(Var(id2), phi))"
*)
type tm_dur = Dur of tm_var * fm_disj_notless
and fm_conj_notless_atom = True | Prop of prop | Equal of tm_var * tm_dur | ULess of fm_disj * fm_disj | E of fm_disj
and fm_conj_notless = And of fm_conj_notless * fm_conj_notless_atom | Not of fm_conj_notless_atom
and fm_disj_notless = Or of fm_conj_notless * fm_disj_notless | Conj of fm_conj_notless


(*
	type conversion functions
*)

(* From fm_disj to fm_disj_less *)
let fm_disj_to_fm_disj_less fm =
	fm


(* From fm_disj to fm_disj_notless *)
let fm_disj_to_fm_disj_notless (fm: fm_disj) : fm_disj_notless  = 
	match fm with
	_ -> Conj(Not(Prop("a"))) 


(* From fm_disj to fm_disj_ex *)
let fm_disj_to_fm_disj_ex (fm: fm_disj) =
	fm

