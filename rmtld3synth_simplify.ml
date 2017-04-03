(*pp camlp4o `ocamlfind query type_conv`/pa_type_conv.cma  `ocamlfind query pa_sexp_conv`/pa_sexp_conv.cma  -I `ocamlfind query sexplib` -I `ocamlfind query pa_sexp_conv` *)

open Batteries
open Map
open Sexplib


open Rmtld3
open Rmtld3_extension
open Rmtld3synth_helper
open Mathkernel

module RepMap = Map.Make(String);;

module SplitFormulaMap = Map.Make(String);;

type mapkeyformula = KUntil of time * formula * formula | KDuration of formula * term | KFormula of formula
type kmap = KVar of var_id | KProp of prop with sexp

let get_unique_id () = let id_count = ref 1 in id_count := !id_count + 1; (string_of_int !id_count)

(* continue here .... converting it to fm_disj_ex *)
let rec repl_tm (tm: tm_disj) (rep_tab: mapkeyformula SplitFormulaMap.t ref) : term =
  match tm with
  | C value            -> Constant(value)
  | Var id             -> Variable(id)
  | Dur (trm,sf)       -> let o_fm = rpl_disj_fm sf rep_tab in
    let o_tm = repl_tm trm rep_tab in

    let varid = ("tm" ^ (get_unique_id ())) in

    rep_tab := SplitFormulaMap.add varid (KDuration(o_fm,o_tm)) !rep_tab; (* add formula and sub-term *)

    (* duration terms are replaced with free variables *)
    Variable(varid)

  | Plus (eta1,eta2)   -> FPlus(repl_tm eta1 rep_tab, repl_tm eta2 rep_tab)
  | Times (eta1,eta2)  -> FTimes(repl_tm eta1 rep_tab, repl_tm eta2 rep_tab)

and rpl_atoms_fm (fm: atoms) (rep_tab: mapkeyformula SplitFormulaMap.t ref) : formula =
  match fm with
  | Prop p                 -> Prop(p)
  | ULess (pval, sf1, sf2) -> let o_fm1 = rpl_disj_fm sf1 rep_tab in
    let o_fm2 = rpl_disj_fm sf2 rep_tab in

    let varid = ("fm" ^ (get_unique_id ()) ) in

    rep_tab := SplitFormulaMap.add varid (KUntil(pval,o_fm1,o_fm2)) !rep_tab;

    (* until operators are replaced by propositions *)
    Prop(varid)

  | E (var,sf)             -> Exists(var, rpl_disj_fm sf rep_tab)
  | Less (tr1,tr2)         -> LessThan(repl_tm tr1 rep_tab, repl_tm tr2 rep_tab)

and rpl_conj_fm (fm_conj: fm_conj) (rep_tab: mapkeyformula SplitFormulaMap.t ref) : formula =
  match fm_conj with
    And(fm1,fm2) -> Or(rpl_conj_fm fm1 rep_tab, rpl_atoms_fm fm2 rep_tab)
  | Not(fm)      -> Not(rpl_atoms_fm fm rep_tab)

and rpl_disj_fm (fm_disj: fm_disj) (rep_tab: mapkeyformula SplitFormulaMap.t ref) : formula =
  match fm_disj with
  | Or(fm1,fm2) -> Or(rpl_disj_fm fm1 rep_tab, rpl_conj_fm fm2 rep_tab)
  | Conj(fm)    -> rpl_conj_fm fm rep_tab


let rec rmtld_tm_of_map_fm (tm: rmtld3_tm) (mapfm: mapkeyformula SplitFormulaMap.t) : rmtld3_tm =
  match tm with
  | Variable(id) ->
    begin (* is the variable id replaced ? *)
      try
        match SplitFormulaMap.find id mapfm with
        | KDuration(sf,trm) -> Duration (trm,sf)
        | _                 -> raise (Failure ("rmtld_fm_of_map_fm error: no KDuration"))
      with Not_found -> Variable(id)
    end

  | _            -> raise (Failure ("bad unreplace: " ^ (string_of_rmtld_tm tm)))

and rmtld_fm_of_map_fm (fm: rmtld3_fm) (mapfm: mapkeyformula SplitFormulaMap.t) : rmtld3_fm = 
  (* replace identified free variables and temporal operators into the formula fm *)
  match fm with
  | True()                 -> True()
  | Not(Or(LessThan(Constant(1.), Variable(varid1)), LessThan(Variable(varid2), Constant(1.)))) when varid1 = varid2 ->
    (* get available sub-formula *)
    (
    match SplitFormulaMap.find varid1 mapfm with
    | KUntil(pval,fm1,fm2) -> Until(pval, (rmtld_fm_of_map_fm fm1 mapfm), (rmtld_fm_of_map_fm fm2 mapfm))
    | _                    -> raise (Failure ("rmtld_fm_of_map_fm error: no KUntil"))
    )


  | LessThan(tm1,tm2)      -> LessThan(rmtld_tm_of_map_fm tm1 mapfm, rmtld_tm_of_map_fm tm2 mapfm)

  | Prop p                 -> Prop(p)
  | Not sf                 -> Not(rmtld_fm_of_map_fm sf mapfm)
  | Or (sf1, sf2)          -> Or(rmtld_fm_of_map_fm sf1 mapfm, rmtld_fm_of_map_fm sf2 mapfm)
  | Exists (var,sf)        -> Exists(var, rmtld_fm_of_map_fm sf mapfm)
  | _                      -> raise (Failure ("bad unreplace: " ^ (string_of_rmtld_fm fm)))


(*
	helpers for map_fm sets
*)
let string_of_fm_map fm_map =
  let stringify = fun ky value str ->
    match value with
      KUntil(pval,fm1,fm2) -> str ^ (ky ^ " -> Until " ^ (string_of_float pval) ^ " " ^ (string_of_rmtld_fm fm1) ^ " " ^ (string_of_rmtld_fm fm2) ^ "\n")
    | KDuration(fm,tm)     -> str ^ (ky ^ " -> Duration " ^ (string_of_rmtld_fm fm) ^ " " ^ (string_of_rmtld_tm tm) ^ "\n" )
    | KFormula(fm)         -> str ^ (ky ^ " -> Formula " ^ (string_of_rmtld_fm fm) ^ "\n" )
  in
  SplitFormulaMap.fold (stringify) fm_map ""

let print_fm_map fm_map = print_endline (string_of_fm_map fm_map)



let simplify (rmtld_formula: rmtld3_fm) : rmtld3_fm = 
  verb_m 1 (fun _ -> print_endline "Simplification enabled.";);

  (* open  mathematica task to aid simplification *)
  Mathkernel.mk_handshake ();

  (* test if mathematica has been correctly initalized *)
  Mathkernel.mk_writeln "$Version";
  let answer = Mathkernel.mk_readln () in
  if answer <> "" then
    verb_m 1 (fun _ -> print_endline ("Mathematica has been initialized sucessfully with version "^(String.strip answer)^".");)
  else raise (Failure ("Mathematica has not been initialized properly."));



  let dnf_fm fm = Mathkernel.m_fm_to_rmtld (Mathkernel.m_fm_dnf (Mathkernel.rmtld_fm_to_m fm)) in
  let select lst = (List.hd lst, List.tl lst) in

  let rec split_conj (fm_conj: formula) = match fm_conj with Not(Or(Not(a),Not(b))) -> (split_conj a) @ (split_conj b) | a -> [a]  in
  let rec split_disj (fm_disj: formula) = match fm_disj with Or(a,b) -> (split_disj a) @ (split_disj b) | a -> [a] in

  let isol lst = List.fold_left (fun (wineq,nineq) a -> if try fm_disj_to_fm_disj_notless (formula_to_fm_disj a); false with _ -> true then (a::wineq,nineq) else (wineq,a::nineq) ) ([],[]) lst in
  let isol_conj fm_conj = isol (split_conj fm_conj) in
  let isol_disj fm_disj = isol (split_disj fm_disj)  in

  let lst_to_conj fm_conj_lst = if List.length fm_conj_lst > 0 then dnf_fm (List.fold_left (fun (a: formula) b -> Not(Or(Not(a),Not(b)))) (List.hd fm_conj_lst) (List.tl fm_conj_lst)) else raise (Failure ("lst_to_conj from an empty list")) in

  let concat_dnf_fm fm1 fm2 = dnf_fm (Or(fm1,fm2)) in
  let lst_to_dnf lst = if List.length lst > 0 then dnf_fm (List.fold_left (fun (a: formula) b -> Or(a,b)) (List.hd lst) (List.tl lst)) else raise (Failure ("lst_to_dnf from an empty list")) in


  (* Axioms:
       1) phi1 or ( phir and phi2 ) U phi3 -> [ ( phir -> (phi1 or phi2) U phi3 ) and (not phir -> phi1 U phi3)]
       2) phi1 U ((phir and phi2) or phi3) -> (phir -> phi1 U (phi2 or phi3)) and (not phir -> phi1 U phi3)
       3) always \int^eta (phi1 or phi2) = \int^eta phi1 + \int^eta phi2 - \int^eta (phi1 and phi2)
   *)
  let axiom1_primitive pval fm1 fm_r fm2 fm3 =
    (* Axiom 1 - phi1 or ( phir and phi2 ) U phi3 -> [ ( phir -> (phi1 or phi2) U phi3 ) and (not phir -> phi1 U phi3)] *)
    mand (mimplies fm_r ( Until(pval, Or(fm1,fm2), fm3) )) (mimplies (Not(fm_r)) (Until(pval,fm1,fm3)))
  in
  let axiom2_primitive pval fm3 fm_r fm2 fm1 =
    (* Axiom 2 - phi1 U ((phir and phi2) or phi3) -> (phir -> phi1 U (phi2 or phi3)) and (not phir -> phi1 U phi3)) *)
    mand (mimplies (fm_r) ( Until(pval, fm1, Or(fm2,fm3)) )) (mimplies (Not(fm_r)) ( Until(pval, fm1, fm3) ))
  in

  let apply_axiom axiom_primitive pval dnf_fm lst_dnf_wineq fm =

    (* select one disjuntion with inequalities *)
    let item,lst_dnf_wineq_remain = select lst_dnf_wineq in
    let fm_wineq_lst, fm_nineq_lst = isol_conj item in
    let fm_wineq, fm_nineq = (lst_to_conj fm_wineq_lst, lst_to_conj fm_nineq_lst) in

    axiom_primitive pval (concat_dnf_fm (dnf_fm) (lst_to_dnf lst_dnf_wineq_remain)) fm_wineq fm_nineq fm
  in

  let replace_fm key fm =
    match fm with
      KUntil(pval,fm1,fm2) -> let map_fm = (rpl_disj_fm (formula_to_fm_disj fm1)) in
      let map_fm2 = (rpl_disj_fm (formula_to_fm_disj fm2)) in
      SplitFormulaMap.empty
    | _                    -> SplitFormulaMap.empty

  in

  let join_map_fm map_fm1 map_fm2 = SplitFormulaMap.fold (fun key value map -> SplitFormulaMap.add key value map) map_fm1 map_fm2 in


  (* Heuristic for simplifcation of until operators and duration terms:

     1) Create two map functions that stores, repectivelly, until operators and duration terms

     2) Chose a duration term
        - case duration term contains variables to be replaced that are not marked as solved then try to solve it first
        - case duration term does not contain any variable or all available variables are marked as solved:
           2.1) replace all
           2.2) apply axiom 4 and marl current duration term as solved

     3) Chose an until operator.
        - case until operator contains variables to be replaced that are not marked as solved then try to solve unsolved replacements first
        - case until operator does not contain any variable or all available variables are marked as solved:
           3.1) replace all
           3.2) apply axiom 1 followed by axiom 2 until no more Less constructors exists inside the chosen temporal operator and mark current until operator as solved

   *)

  let simplify_heuristic splited_formula =
    let f key tuple (unsolved_map_fm, solved_map_fm) =
        let rec vars_solved_in_tm tm unsol_map sol_map =
          (* search if all formula variables are solved; return true if yes; no otherwise. *)
          match tm with
          | Constant(x)        -> true
          | Variable(id)       -> (try SplitFormulaMap.find (Sexp.to_string (sexp_of_kmap (KVar(id)))) unsol_map; false with Not_found -> true)
          | FPlus (eta1,eta2)  -> (vars_solved_in_tm eta1 unsol_map sol_map) && (vars_solved_in_tm eta2 unsol_map sol_map)
          | FTimes (eta1,eta2) -> (vars_solved_in_tm eta1 unsol_map sol_map) && (vars_solved_in_tm eta2 unsol_map sol_map)
          | _ -> raise (Failure ("vars_solved_in_tm applied to unreplaced duration"))
        and vars_solved_in_fm (fm: formula) unsol_map sol_map =
          match fm with
          | Prop p                 -> (try SplitFormulaMap.find (Sexp.to_string (sexp_of_kmap (KProp(p)))) unsol_map; false with Not_found -> true)
          | Not sf                 -> vars_solved_in_fm sf unsol_map sol_map
          | Or (sf1, sf2)          -> (vars_solved_in_fm sf1 unsol_map sol_map) && (vars_solved_in_fm sf2 unsol_map sol_map)
          | Exists (var,sf)        -> vars_solved_in_fm sf unsol_map sol_map
          | LessThan (tr1,tr2)     -> (vars_solved_in_tm tr1 unsol_map sol_map) && (vars_solved_in_tm tr2 unsol_map sol_map)
          | _ -> raise (Failure ("vars_solved_in_fm applied to unreplaced temporal operator"))
        in
        let isol_until_oper pval fm1 fm2 =
          (* put fm1 in disjuntive normal form and isolate inequalities *)
          let lst_dnf_nineq,lst_dnf_wineq = isol_disj (dnf_fm fm1) in

          if lst_dnf_wineq <> [] then
            KFormula(apply_axiom axiom1_primitive pval (lst_to_dnf lst_dnf_nineq) (lst_dnf_wineq) fm2)
          else
            begin
              (* put fm2 in DNF and isolate inequalities *)
              let lst_dnf_nineq2,lst_dnf_wineq2 = isol_disj (dnf_fm fm2) in

              if lst_dnf_wineq2 <> [] then
                KFormula(apply_axiom axiom2_primitive pval (lst_to_dnf lst_dnf_nineq2) (lst_dnf_wineq2) fm1)
              else
                KUntil(pval,fm1,fm2)
            end

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
              let unsolved_fm_x = replace_fm key out_fm in

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

        | _ -> raise (Failure ("simplify_heuristic applied to unknown tokens"))
    in

    if splited_formula = SplitFormulaMap.empty then
      raise (Failure ("simplify_heuristic of an empty formula"))
    else
      begin
        let unsolved_fm, solved_fm = SplitFormulaMap.fold f splited_formula (SplitFormulaMap.empty, SplitFormulaMap.empty) in
        if unsolved_fm = SplitFormulaMap.empty then solved_fm else raise (Failure ("simplify_heuristic contains unsolved formulas"))
      end
  in


  

(*
	let apply_axioms_for_until_op_isolation_sub phi =
		match phi with
		(* Axiom two *)
		| Until( t, phi1, Or(Not(Or(Not(phir), Not(phi2))), phi3)) -> apply_axiom2_for_until_op_isolation t phi1 phi2 phi3 phir
		| Until( t, phi1, Not(Or(Not(phir), Not(phi2)))) -> apply_axiom2_for_until_op_isolation t phi1 phi2 mfalse phir
		| Until( t, phi1, Or(Not(phir), phi3)) -> apply_axiom2_for_until_op_isolation t phi1 mtrue phi3 phir
		| Until( t, phi1, phir) -> apply_axiom2_for_until_op_isolation t phi1 mtrue mfalse phir

		| phi1 -> phi1

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
*)


  (* close mathematica process *)
  Mathkernel.close_process_mathematica ();

  (* PUT HERE THE RESULT of the simplification phase *)
  
  (* type conversion from rmtld3_fm to fm_disj_ex *)
  let map_ex = ref  SplitFormulaMap.empty in
  let fm_dsj_ex = rpl_disj_fm (formula_to_fm_disj rmtld_formula) map_ex in
  map_ex := SplitFormulaMap.add "mainfm" (KFormula(fm_dsj_ex)) !map_ex;

  verb (fun _ -> print_fm_map !map_ex);
  
  (* apply heuristic *)
  let fm_map_solved = simplify_heuristic !map_ex in

  (* get KFormula list *)
  let get_lst_kformula map : rmtld3_fm list = SplitFormulaMap.fold (fun key tuple b -> match tuple with KFormula(fm) -> fm::b | _ -> b ) map [Not(True())] in
  let fm_lst = get_lst_kformula fm_map_solved in
  (* list shall be of size one *)
  if List.length fm_lst <> 1 then raise (Failure ("simplify: list shall be of size 1")) else
  rmtld_fm_of_map_fm (List.hd fm_lst) fm_map_solved
  

  

