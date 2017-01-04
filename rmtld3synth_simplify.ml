(*pp camlp4o `ocamlfind query type_conv`/pa_type_conv.cma  -I `ocamlfind query sexplib` -I `ocamlfind query pa_sexp_conv` pa_sexp_conv.cma *)

open Map
open Sexplib

open Rmtld3
(*open Mathkernel*)

module RepMap = Map.Make(String);;

module SplitFormulaMap = Map.Make(String);;

type mapkeyformula = KUntil of time * formula * formula | KDuration of formula * term | KFormula of formula
type kmap = KVar of var_id | KProp of prop with sexp


let rec replace_rmtld_tm rmtld_term rep_tab =
	match rmtld_term with
    | Constant value      -> Constant(value)
    | Variable id         -> Variable(id)
    | Duration (trm,sf)   -> let o_fm = replace_rmtld_fm sf rep_tab in
    						 let o_tm = replace_rmtld_tm trm rep_tab in
    						 let varid = "aa" in (* TODO ... replace names automatically *)
    						 (*Duration(trm,sf)*)
    						 (fst rep_tab) := RepMap.add varid (o_fm,o_tm) !(fst rep_tab); (* add formula and sub-term *)
    						 Variable(varid)

    | FPlus (eta1,eta2)   -> FPlus(replace_rmtld_tm eta1 rep_tab, replace_rmtld_tm eta2 rep_tab)
    | FTimes (eta1,eta2)  -> FTimes(replace_rmtld_tm eta1 rep_tab, replace_rmtld_tm eta2 rep_tab)

and replace_rmtld_fm rmtld_formula rep_tab =
	match rmtld_formula with
    | Prop p                 -> Prop(p)
    | Not sf                 -> Not(replace_rmtld_fm sf rep_tab)
    | Or (sf1, sf2)          -> Or(replace_rmtld_fm sf1 rep_tab, replace_rmtld_fm sf2 rep_tab)
    | Until (pval, sf1, sf2) -> let o_fm1 = replace_rmtld_fm sf1 rep_tab in
    							let o_fm2 = replace_rmtld_fm sf2 rep_tab in
    							let varid = "bb" in (* TODO ... replace names automatically *)
    							(snd rep_tab) := RepMap.add varid (pval,o_fm1,o_fm2) !(snd rep_tab);
    							(*Until(pval,replace_rmtld_fm sf1 rep_tab, replace_rmtld_fm sf2 rep_tab)*)
    							Not(Or(LessThan(Constant(1.), Variable(varid)), LessThan(Variable(varid), Constant(1.))))
    | Exists (var,sf)        -> Exists(var, replace_rmtld_fm sf rep_tab)
    | LessThan (tr1,tr2)     -> LessThan(replace_rmtld_tm tr1 rep_tab, replace_rmtld_tm tr2 rep_tab)


let rec unreplace_rmtld_tm rmtld_tm m_map =
	match rmtld_tm with
	| Variable(id) ->
		begin (* is the variable id replaced ? *)
			try let sf,trm = RepMap.find id !(fst m_map) in
			Duration (trm,sf) with Not_found -> Variable(id)
		end

	| _ -> raise (Failure ("bad unreplace: " ^ (string_of_rmtld_tm rmtld_tm)))

and unreplace_rmtld_fm rmtld_fm m_map = 
	(* unreplace identified free variables and replaced temporal operators *)
	match rmtld_fm with
	| Not(Or(LessThan(Constant(1.), Variable(varid1)), LessThan(Variable(varid2), Constant(1.)))) ->
			if varid1 = varid2 then
			begin
			 	(* get available sub-formula *)
			 	let pval,fm1,fm2 = RepMap.find varid1 !(snd m_map) in
			 	Until(pval, (unreplace_rmtld_fm fm1 m_map), (unreplace_rmtld_fm fm2 m_map))
			end
			else
			raise (Failure ("bad unreplace: " ^ (string_of_rmtld_fm rmtld_fm)))
	
	| LessThan(tm1,tm2) -> LessThan(unreplace_rmtld_tm tm1 m_map, unreplace_rmtld_tm tm2 m_map)

	| Prop p                 -> Prop(p)
    | Not sf                 -> Not(unreplace_rmtld_fm sf m_map)
    | Or (sf1, sf2)          -> Or(unreplace_rmtld_fm sf1 m_map, unreplace_rmtld_fm sf2 m_map)
    | Exists (var,sf)        -> Exists(var, unreplace_rmtld_fm sf m_map)
	| _ -> raise (Failure ("bad unreplace: " ^ (string_of_rmtld_fm rmtld_fm)))

let rec is_rigid_tm rmtld_tm =
	(* a rigid term is a term containing only constants, variables with plus and times operators *)
	match rmtld_tm with
	| Constant(x)        -> true
	| Variable(id)       -> true
	| FPlus (eta1,eta2)  -> true && (is_rigid_tm eta1) && (is_rigid_tm eta2)
	| FTimes (eta1,eta2) -> true && (is_rigid_tm eta1) && (is_rigid_tm eta2)
	| _ -> false

and is_rigid_fm rmtld_fm =
	(* a rigid formula is a formula that only contain rigid terms *)
	match rmtld_fm with
	| Prop p                 -> false
  	| Not sf                 -> is_rigid_fm sf
  	| Or (sf1, sf2)          -> (is_rigid_fm sf1) || (is_rigid_fm sf2)
  	| Until (pval, sf1, sf2) -> (is_rigid_fm sf1) || (is_rigid_fm sf2)
  	| Exists (var,sf)        -> is_rigid_fm sf
  	| LessThan (tr1,tr2)     -> (is_rigid_tm tr1) && (is_rigid_tm tr2)

let simplify rmtld_formula = 
	print_endline "Simplification enabled.";

	(* call mathematica to simplify this formula *)
	Mathkernel.mk_handshake ();

	(* search for free variables *)

	(* replace each duration with a new free variable and temporal operators with '<free variable>=0' *)
	let in_map_pair = (ref RepMap.empty, ref RepMap.empty) in
	let rep_formula = replace_rmtld_fm rmtld_formula in_map_pair in

	(* debug replacement phase *)
	let o_ref_map1,o_ref_map2 = in_map_pair in
	RepMap.iter (fun ky (val1,val2) -> Printf.printf "%s -> %s %s\n" ky (string_of_rmtld_fm val1) (string_of_rmtld_tm val2)) !o_ref_map1;
	RepMap.iter (fun ky (pval,val1,val2) -> Printf.printf "%s -> %s | %s | %s\n" ky (string_of_float pval) (string_of_rmtld_fm val1) (string_of_rmtld_fm val2)) !o_ref_map2;

	print_plaintext_formula rep_formula;

	
	(* turn formula to CNF
	   A formula is in CNF if subformulas of duration terms and temporal operators are in CNF.
	*)
	let fm_to_cnf f =
		let mathematica_formula_cnf = Mathkernel.m_fm_cnf (Mathkernel.rmtld_fm_to_m f) in

		let x = Sexp.to_string (Mathkernel.sexp_of_m_fm mathematica_formula_cnf) in
		print_endline ("mathematica formula: " ^ x);

		Mathkernel.m_fm_to_rmtld mathematica_formula_cnf
	in

	let rmtld_formula_cnf = fm_to_cnf rep_formula in


	(* turn each replaced formula into CNF *)
	o_ref_map1 := RepMap.fold (fun ky (val1,val2) rf -> RepMap.add ky (fm_to_cnf val1, val2) rf) !o_ref_map1 RepMap.empty ;
	o_ref_map2 := RepMap.fold (fun ky (pval,val1,val2) rf -> RepMap.add ky (pval, fm_to_cnf val1, fm_to_cnf val2) rf) !o_ref_map2 RepMap.empty;



	(* unreplace phase - reconstruction *)
	let rmtld_formula_unrep = unreplace_rmtld_fm rmtld_formula_cnf (o_ref_map1,o_ref_map2) in
	print_plaintext_formula rmtld_formula_unrep;

	
	(* 
		formula rmtld_formula_unrep is now in CNF
		let's try to apply the axioms with the proper heuristic
	*)

	(* FIRST lifting HEURISTIC for until operators:

		1) Create two map functions that stores, repectivelly, until operators and duration terms

		2) Chose a duration term
			- case duration term contains variables to be replaced that are not marked as solved then try to solve it first
			- case duration term does not contain any variable or they are marked as solved:
				2.1) replace all
				2.2) apply axiom 4

		3) Chose an until operator.
			- case until operator contains variables to be replaced that are not marked as solved then try to solve unsolved replacements first
			- case until operator contains every variable to be replaced marked as solved
			  	3.1) replace all
				3.2) apply axiom 1 followed by axiom 2 until no more Less constructs exists inside the temporal operator and then
					 mark current until operator as solved

	*)

	let select lst = (List.hd lst, List.tl lst) in
	let dnf_fm fm = Mathkernel.m_fm_to_rmtld (Mathkernel.m_fm_dnf (Mathkernel.rmtld_fm_to_m fm)) in

	let rec split_conj fm_conj = match fm_conj with Not(Or(Not(a),Not(b))) -> (split_conj a) @ (split_conj b) | a -> [a]  in
	let rec split_disj fm_disj = match fm_disj with Or(a,b) -> (split_disj a) @ (split_disj b) | a -> [a] in
	
	let isol lst = List.fold_left (fun (wineq,nineq) a -> if try Rmtld3_extension.fm_disj_to_fm_disj_notless a; false with _ -> true then (a::wineq,nineq) else (wineq,a::nineq) ) ([],[]) lst in
	let isol_conj fm_conj = isol (split_conj fm_conj) in
	let isol_disj fm_disj = isol (split_disj fm_disj)  in

	let lst_to_conj fm_conj_lst = if List.length fm_conj_lst > 0 then dnf_fm (List.fold_left (fun a b -> Not(Or(Not(a),Not(b)))) (List.hd fm_conj_lst) (List.tl fm_conj_lst)) else raise (Failure ("lst_to_conj from an empty list")) in

	let concat_dnf_fm fm1 fm2 = dnf_fm (Or(fm1,fm2)) in
	let lst_to_dnf lst = if List.length lst > 0 then dnf_fm (List.fold_left (fun a b -> Or(a,b)) (List.hd lst) (List.tl lst)) else raise (Failure ("lst_to_dnf from an empty list")) in

	let apply_axiom1 pval dnf_fm lst_dnf_wineq fm3 =
		let apply_axiom1_primitive fm1 fm_r fm2 fm3 =
			(* Axiom 1 - phi1 or ( phir and phi2 ) U phi3 -> [ ( phir -> (phi1 or phi2) U phi3 ) and (not phir -> phi1 U phi3)] *)
			mand (mimplies fm_r ( Until(pval, Or(fm1,fm2), fm3) )) (mimplies (Not(fm_r)) (Until(pval,fm1,fm3)))
		in

		(* select one disjuntion *)

		let item,lst_dnf_wineq_remain = select lst_dnf_wineq in
		let fm_wineq_lst, fm_nineq_lst = isol_conj item in
		let fm_wineq, fm_nineq = (lst_to_conj fm_wineq_lst, lst_to_conj fm_nineq_lst) in

		apply_axiom1_primitive (concat_dnf_fm (dnf_fm) (lst_to_dnf lst_dnf_wineq_remain)) fm_wineq fm_nineq fm3

	in

	let replace_until_fm fm =
		SplitFormulaMap.empty
	in

	let join_map_fm map_fm1 map_fm2 = SplitFormulaMap.fold (fun key value map -> SplitFormulaMap.add key value map) map_fm1 map_fm2 in

	let lifting_heuristic splited_formula =
		let f = (fun key tuple (unsolved_map_fm, solved_map_fm) ->
			let isol_until_oper pval fm1 fm2 =
				(* put fm1 in disjuntive normal form *)
				(* classify each disjuntion as isolated and nonisolated; for each non isolated disjuntion apply axiom 1 *)
				(*
					1) select a non isolated disjunction;
					2) apply axiom 1 until no more non isolated disjutions exists
				*)

				let lst_dnf_nineq,lst_dnf_wineq = isol_disj (dnf_fm fm1) in

				if lst_dnf_wineq <> [] then
					KFormula(apply_axiom1 pval (lst_to_dnf lst_dnf_nineq) (lst_dnf_wineq) fm2)
				else
				begin
					(* if fm1 is okay then move to fm2 *)
					(* put fm2 in DNF as well *)
					(* do the same as for the first formula *)

					let lst_dnf_nineq2,lst_dnf_wineq2 = isol_disj (dnf_fm fm2) in

					if lst_dnf_wineq2 <> [] then
						KFormula(Prop("XXXX"))
					else
						KUntil(pval,fm1,fm2)
				end
			
			in
			let rec vars_solved_in_tm tm unsol_map sol_map =
				(* search if all formula variables are solved; return true if yes; no otherwise. *)
				match tm with
				| Constant(x)        -> true
				| Variable(id)       -> (try SplitFormulaMap.find (Sexp.to_string (sexp_of_kmap (KVar(id)))) unsol_map; false with Not_found -> true)
				| FPlus (eta1,eta2)  -> (vars_solved_in_tm eta1 unsol_map sol_map) && (vars_solved_in_tm eta2 unsol_map sol_map)
				| FTimes (eta1,eta2) -> (vars_solved_in_tm eta1 unsol_map sol_map) && (vars_solved_in_tm eta2 unsol_map sol_map)
				| _ -> raise (Failure ("vars_solved_in_tm applied to unreplaced duration"))
			and vars_solved_in_fm fm unsol_map sol_map =
				match fm with
				| Prop p                 -> (try SplitFormulaMap.find (Sexp.to_string (sexp_of_kmap (KProp(p)))) unsol_map; false with Not_found -> true)
  				| Not sf                 -> vars_solved_in_fm sf unsol_map sol_map
  				| Or (sf1, sf2)          -> (vars_solved_in_fm sf1 unsol_map sol_map) && (vars_solved_in_fm sf2 unsol_map sol_map)
  				| Exists (var,sf)        -> vars_solved_in_fm sf unsol_map sol_map
  				| LessThan (tr1,tr2)     -> (vars_solved_in_tm tr1 unsol_map sol_map) && (vars_solved_in_tm tr2 unsol_map sol_map)
  				| _ -> raise (Failure ("vars_solved_in_fm applied to unreplaced temporal operator"))
			in
			
			match tuple with
			  KUntil(pval, fm1, fm2) ->  (* until operator case *)
	  				(* - case until operator contains variables to be replaced that are not marked as solved then try to solve unsolved replacements first *)
					begin
	  				match ((vars_solved_in_fm fm1 unsolved_map_fm solved_map_fm),(vars_solved_in_fm fm2 unsolved_map_fm solved_map_fm)) with
	  				  true,true -> (*add current formula to the solved map function *)
	  				  				(* let us begin isolating this until operator *)

	  				  				(*let new_prop = "AAA" in (* TODO prop *)
	  				  				let fm_new = Until(pval, fm1, fm2) in
	  				  				let fm_new2 = Prop(new_prop) in*)

	  				  				let out_fm = isol_until_oper pval fm1 fm2 in

	  				  				(* replace until operators with new propositions and simplify the remaining formula *)
	  				  				let unsolved_fm_x = replace_until_fm out_fm in

	  				  				if unsolved_fm_x <> SplitFormulaMap.empty then
	  				  					(* add new replacements to the unsolved map as well as the remaining formula *)
	  				  					(join_map_fm unsolved_fm_x unsolved_map_fm, solved_map_fm)
	  				  				else
	  				  					(unsolved_map_fm, SplitFormulaMap.add key (out_fm) solved_map_fm)

	  				  				(* fm1 and fm2 need to be isolated at this point *)
	  				  				(*let smfm = SplitFormulaMap.add (Sexp.to_string (sexp_of_kmap (KProp(new_prop)))) (KFormula(fm_new)) solved_map_fm in*)


	  				| true,_ -> (* right side of the until operator need to be solved *)
	  							(unsolved_map_fm, solved_map_fm)

	  				| _,true -> (* left side of the until operator need to be solved *)
	  							(unsolved_map_fm, solved_map_fm)

	  				| false,false -> (* both until operator sub-formulas need to be solved *)
	  								(unsolved_map_fm, solved_map_fm)
	  				end

			| KDuration(fm, tm) ->  (* duration term case *)
					(* - case duration term contains variables to be replaced that are not marked as solved then try to solve it first *)
					begin
					(* consider the term case TODO *)
					match ((vars_solved_in_fm fm unsolved_map_fm solved_map_fm), (vars_solved_in_tm tm unsolved_map_fm solved_map_fm)) with
					  true,_ ->
						(* add current duop to the solved_map *)
						(unsolved_map_fm, SplitFormulaMap.add key tuple solved_map_fm)
					| false,_ ->
						(* add duop as unsolved for a later re-try *)
						(SplitFormulaMap.add key tuple unsolved_map_fm, solved_map_fm)
					end

			| KFormula(fm) -> 	(* main formula *)
					(unsolved_map_fm, solved_map_fm)

			| _ -> raise (Failure ("lifting_heuristic applied to unknown tokens"))
		) in

		if splited_formula = SplitFormulaMap.empty then
			raise (Failure ("lifting_heuristic of an empty formula"))
		else
		begin
			let unsolved_fm, solved_fm = SplitFormulaMap.fold f splited_formula (SplitFormulaMap.empty, SplitFormulaMap.empty) in
			if unsolved_fm = SplitFormulaMap.empty then solved_fm else raise (Failure ("lifting_heuristic contains unsolved formulas"))
		end
	in

	
	(* try to apply axioms to until operators as follows:
		1) phi1 or ( phir and phi2 ) U phi3 -> [ ( phir -> (phi1 or phi2) U phi3 ) and (not phir -> phi1 U phi3)]
		2) phi1 U ((phir and phi2) or phi3) -> (phir -> phi1 U (phi2 or phi3)) and (not phir -> phi1 U phi3)
		3) always \int^eta (phi1 or phi2) = \int^eta phi1 + \int^eta phi2 - \int^eta (phi1 and phi2)
	*)

	let apply_axiom2_for_until_op_isolation t phi1 phi2 phi3 phir =
		(* is phir a rigid formula ? If yes continue. If not trigger a conflit. *)
		if is_rigid_fm phir then
		begin
			print_endline "apply axiom 2";
			(* yes case *)
			Not(Or( Not( Or( Not(phir) , Until(t, phi1, Or(phi2, phi3)) ) ), Not(Or(Not(phir), Until(t, phi1, phi3)))))
		end
		else
			(* no case. we cannot apply it *)
			Until( t, phi1, Or(Not(Or(Not(phir), Not(phi2))), phi3))

	in

	let apply_axioms_for_until_op_isolation_sub phi =
		match phi with
		(* Axiom two *)
		| Until( t, phi1, Or(Not(Or(Not(phir), Not(phi2))), phi3)) -> apply_axiom2_for_until_op_isolation t phi1 phi2 phi3 phir
		| Until( t, phi1, Not(Or(Not(phir), Not(phi2)))) -> apply_axiom2_for_until_op_isolation t phi1 phi2 mfalse phir
		| Until( t, phi1, Or(Not(phir), phi3)) -> apply_axiom2_for_until_op_isolation t phi1 mtrue phi3 phir
		| Until( t, phi1, phir) -> apply_axiom2_for_until_op_isolation t phi1 mtrue mfalse phir

		| phi1 -> phi1

	in

	let apply_axiom1_for_until_op_isolation t phi1 phi2 phi3 phir =
		(* is phir a rigid formula ? If yes conitnue. If not trigger a conflit. *)
		if is_rigid_fm phir then
		begin
			print_endline "apply axiom 1";
			(* yes case *)
			Not(Or(Not(Or(Not(phir), (
				apply_axioms_for_until_op_isolation_sub (Until(t,Or(phi1,phi2),phi3))
			) )), Not(Or(Not(Not(phir)), (
				apply_axioms_for_until_op_isolation_sub (Until(t,phi1,phi2))
			) ))))
		end
		else
			(* no case. we cannot apply it *)
			Until( t, Or( phi1, Not(Or( Not(phir), Not(phi2) ))), phi3 )

	in


	let apply_axioms_for_until_op_isolation phi =
		match phi with

		(* Axiom one *)
		| Until( t, Or( phi1, Not(Or( Not(phir), Not(phi2) ))), phi3 ) -> apply_axiom1_for_until_op_isolation t phi1 phi2 phi3 phir
		| Until( t, Not(Or( Not(phir), Not(phi2) )), phi3 ) -> apply_axiom1_for_until_op_isolation t mfalse phi2 phi3 phir
		| Until( t, Or( phi1, phir), phi3 ) -> apply_axiom1_for_until_op_isolation t phi1 mtrue phi3 phir
		| Until( t, phir, phi3 ) -> apply_axiom1_for_until_op_isolation t mfalse mtrue phi3 phir

		(*| Not( Until(true, Not( Less(Duration() , Duration() ))) ) *)

		(* other cases are mantained *)
		| phi ->  phi
	in

	let result = apply_axioms_for_until_op_isolation rmtld_formula_unrep in

	print_endline (Sexp.to_string (sexp_of_formula result));


	(* close mathematica process *)
    Mathkernel.close_process_mathematica ();
