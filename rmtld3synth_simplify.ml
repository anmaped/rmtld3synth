(*pp camlp4o `ocamlfind query type_conv`/pa_type_conv.cma  `ocamlfind query pa_sexp_conv`/pa_sexp_conv.cma  -I `ocamlfind query sexplib` -I `ocamlfind query pa_sexp_conv` *)

open Batteries
open Map
open Sexplib
open Sexplib.Conv


open Rmtld3
open Rmtld3_extension
open Rmtld3synth_helper
open Mathkernel


exception Empty_wineq_lst of unit;;

(*
 * convert formula into a map of fm_disj_ex
 *)
let fm_to_fm_disj_ex_map (fm: fm) : map_of_fm_disj_ex =
	(* do a replacement first; than continue with rmtld_fm_to_m *)
  let map = ref Fm_container.empty in
  let tag : idx_ct = KFormula(fm_map_of_fm fm map) in
  add_map "mainfm" tag map;
  verb (fun _ -> print_fm_map !map);

  (* lets put each available formula in DNF *)
  let fld : 'a -> idx_ct -> 'b -> 'b = fun key value b ->
    match value with
      KUntil(pval,fm1,fm2) -> let tag : idx_ct_fm_disj_ex =
                              KUntil(pval,
      	                      fm_disj_to_fm_disj_ex (fm_disj_of_fm fm1),
      	                      fm_disj_to_fm_disj_ex (fm_disj_of_fm fm2)) in
                              Fm_container.add (Sexp.to_string (sexp_of_kmap (KProp(key)))) tag b (* fm_disj_of_fm fm1*)

    | KDuration(fm,tm)     -> let tag : idx_ct_fm_disj_ex =
                              KDuration(
      	                        fm_disj_to_fm_disj_ex (fm_disj_of_fm fm),
      	                        tm_disj_to_tm_disj_ex (tm_disj_of_tm tm)
      	                      ) in
                              Fm_container.add (Sexp.to_string (sexp_of_kmap (KVar(key)))) tag b

    | KFormula(fm)         -> let tag : idx_ct_fm_disj_ex =
                              KFormula(
      	                        fm_disj_to_fm_disj_ex (fm_disj_of_fm fm)
      	                      ) in
                              Fm_container.add (Sexp.to_string (sexp_of_kmap (KForm(key)))) tag b
  in
  Fm_container.fold fld !map Fm_container.empty


(* From Rmtld3.formula to fm_disj_ex *)
let fm_to_fm_disj_ex (fm: fm) : fm_disj_ex =
  (* reconstruct formula *)
  (* convert fm to fm_disj_ex_map and then get fm from fm_disj_ex_map *)
  (* convert fm to fm_disj_ex after doing the mapping *)
  fm_disj_to_fm_disj_ex (fm_disj_of_fm (fm_of_map_fm_disj_ex (fm_to_fm_disj_ex_map fm)))



let fm_to_fm_disj (fm: fm) : fm_disj =
  (* convert from fm_disj_ex to fm_disj *)
  fm_disj_of_fm_disj_ex (fm_to_fm_disj_ex fm)



let simplify (rmtld_formula: fm) : fm = 
  verb_m 1 (fun _ -> print_endline "Simplification enabled.";);

  let isol f lst =
    List.fold_left (fun (nineq,wineq) a ->
      if try f a; false with _ -> true then (nineq,a::wineq) else (a::nineq,wineq)
    ) ([],[]) lst
  in let isol_conj (fm: fm_conj_ex) : (fm_atom_ex list * fm_atom_ex list) =
    isol (fm_atom_notless_of_fm_atom % fm_atom_of_fm_atom_ex) (fm_atom_ex_lst_of_fm_conj_ex fm)
  in let isol_disj (fm: fm_disj_ex) : (fm_conj_ex list * fm_conj_ex list) =
    isol (fm_conj_notless_of_fm_conj % fm_conj_of_fm_conj_ex) (fm_conj_ex_lst_of_fm_disj_ex fm)
  in

  

  (*let dnf_fm fm = Mathkernel.m_fm_to_rmtld (Mathkernel.m_fm_dnf (Mathkernel.rmtld_fm_to_m fm)) in *)
  (*let concat_dnf_fm fm1 fm2 = dnf_fm (Or(fm1,fm2)) in*)
  (*let lst_to_dnf lst = if List.length lst > 0 then dnf_fm (List.fold_left (fun (a: formula) b -> Or(a,b)) (List.hd lst) (List.tl lst)) else raise (Failure ("lst_to_dnf from an empty list")) in*)


  (* Axioms:
       1) phi1 or ( phir and phi2 ) U phi3 -> [ ( phir -> (phi1 or phi2) U phi3 ) and (not phir -> phi1 U phi3)]
       2) phi1 U ((phir and phi2) or phi3) -> (phir -> phi1 U (phi2 or phi3)) and (not phir -> phi1 U phi3)
       3) always \int^eta (phi1 or phi2) = \int^eta phi1 + \int^eta phi2 - \int^eta (phi1 and phi2)
   *)
  let axiom1_primitive pval (fm1: fm_disj_ex) (fm_r: fm_atom_ex) (fm2: fm_disj_ex) (fm3: fm_disj_ex) : fm_disj_ex =
    (* Axiom 1 - phi1 or ( phir and phi2 ) U phi3 -> [ ( phir -> (phi1 or phi2) U phi3 ) and (not phir -> phi1 U phi3)] *)
    let untilA : fm_atom_ex = ULess(pval, tc_disj fm1 fm2, fm3)
    in
    let untilB : fm_atom_ex = ULess(pval, fm1, fm3)
    in
    (* 
        DNF | (phir -> untilA) ∧ (¬phir -> untilB) ==> (untilB ∧ ¬phir) ∨ (phir ∧ untilA)
    *)
    tc_disj ( `Conj(tc_conj (`X(untilB)) (`X(Not(fm_r)))) ) ( `Conj(tc_conj (`X(fm_r)) (`X(untilA))) )
  in
  let axiom2_primitive pval (fm3: fm_disj_ex) (fm_r: fm_atom_ex) (fm2: fm_disj_ex) (fm1: fm_disj_ex) : fm_disj_ex =
    (* Axiom 2 - phi1 U ((phir and phi2) or phi3) -> (phir -> phi1 U (phi2 or phi3)) and (not phir -> phi1 U phi3)) *)
    let untilA : fm_atom_ex = ULess(pval,  fm1, tc_disj fm2 fm3)
    in
    let untilB : fm_atom_ex = ULess(pval, fm1, fm3)
    in
    tc_disj ( `Conj(tc_conj (`X(untilB)) (`X(Not(fm_r)))) ) ( `Conj(tc_conj (`X(fm_r)) (`X(untilA))) )
  in

  let apply_axiom axiom_primitive pval (dnf_fm: fm_disj_ex) (lst_dnf_wineq: fm_conj_ex list) (fm: fm_disj_ex) : fm_disj_ex =

    (* select one disjuntion with inequalities *)
    let item,lst_dnf_wineq_remain = select lst_dnf_wineq in
    let fm_wineq_lst, fm_nineq_lst = isol_conj item in
    (* get one atom from fm_wineq_lst and merge both remainig elements *)
    if fm_wineq_lst <> [] then
      let el,lst = select fm_wineq_lst
      in
      let fm_nineq_lst = fm_nineq_lst @ lst
      in
      let fm_wineq, fm_nineq = (el, `Conj(fm_conj_ex_of_fm_atom_ex_lst fm_nineq_lst)) in

      axiom_primitive pval (tc_disj dnf_fm (fm_disj_ex_of_fm_conj_ex_lst lst_dnf_wineq_remain)) fm_wineq fm_nineq fm
    else
      raise (Empty_wineq_lst ()) (* ###### CONFIRM THIS LINE *)
  in

  let replace_fm key (fm: idx_ct_fm_disj_ex) =
    match fm with
      KUntil(pval,fm1,fm2) ->

(* this is unfinished WRONG !! *)

        (*let map_fm = (rpl_disj_fm (fm_to_fm_disj fm1))
        in let map_fm2 = (rpl_disj_fm (fm_to_fm_disj fm2))
        in*) Fm_container.empty
    | _                    -> Fm_container.empty

  in

  let join_map_fm map_fm1 map_fm2 = Fm_container.fold (fun key value map -> Fm_container.add key value map) map_fm1 map_fm2
  in

  let rec is_fm_disj_ex_solved (fm: fm_disj_ex) unsol_map sol_map =
    (* basically identify if a formula contain RD and RU and if these sub-formulas are also solved *)
    match fm with
    | `Or(cnj,dsj) -> (is_fm_conj_ex_solved cnj unsol_map sol_map) && (is_fm_disj_ex_solved dsj unsol_map sol_map)
    | `Conj(cnj)   -> is_fm_conj_ex_solved cnj unsol_map sol_map

  and is_fm_atom_ex_solved (fm: fm_atom_ex) unsol_map sol_map =
    match fm with
    | Not(at)          -> is_fm_atom_ex_solved at unsol_map sol_map
    | True             -> true
    | Prop(id)         -> true
    | Less(tm1,tm2)    -> (is_tm_disj_ex_solved tm1 unsol_map sol_map) && (is_tm_disj_ex_solved tm2 unsol_map sol_map)
    | Equal(tm1,tm2)   -> (is_tm_disj_ex_solved tm1 unsol_map sol_map) && (is_tm_disj_ex_solved tm2 unsol_map sol_map)
    | EqualD(id,(tm,fm)) -> (is_tm_disj_ex_solved tm unsol_map sol_map) && (is_fm_disj_ex_solved fm unsol_map sol_map)
    | E(id,fm)         -> is_fm_disj_ex_solved fm unsol_map sol_map
    | RU(id)           -> (try Fm_container.find (Sexp.to_string (sexp_of_kmap (KProp(id)))) sol_map; true with Not_found -> false)
    | _ ->  raise (Failure ("is_fm_atom_ex_solved is applied to unreplaced temporal operators"))

  and is_fm_conj_ex_solved (fm: fm_conj_ex) unsol_map sol_map =
    match fm with
    | `And(at,cnj) -> (is_fm_atom_ex_solved at unsol_map sol_map) && (is_fm_conj_ex_solved cnj unsol_map sol_map)
    | `X(at)       -> is_fm_atom_ex_solved at unsol_map sol_map

  and is_tm_disj_ex_solved (tm: tm_disj_ex) unsol_map sol_map =
    match tm with
    | Var(id)        -> true
    | C(v)           -> true
    | Plus(tm1,tm2)  -> (is_tm_disj_ex_solved tm1 unsol_map sol_map) && (is_tm_disj_ex_solved tm2 unsol_map sol_map)
    | Times(tm1,tm2) -> (is_tm_disj_ex_solved tm1 unsol_map sol_map) && (is_tm_disj_ex_solved tm2 unsol_map sol_map)
    | RD(id)         -> (try Fm_container.find (Sexp.to_string (sexp_of_kmap (KVar(id)))) sol_map; true with Not_found -> false)
    | _ -> raise (Failure ("is_tm_disj_ex_solved is applied to unreplaced duration terms"))
  in


  (* Heuristic for simplifcation of until operators and duration terms:

     1) Create two map functions that stores, repectivelly, until operators and duration terms

     2) Chose a duration term
        - case duration term contains variables to be replaced that are not marked as solved then try to solve it first
        - case duration term does not contain any variable or all available variables are marked as solved:
           2.1) replace all
           2.2) apply axiom 4 and mark current duration term as solved

     3) Chose an until operator.
        - case until operator contains variables to be replaced that are not marked as solved then try to solve unsolved replacements first
        - case until operator does not contain any variable or all available variables are marked as solved:
           3.1) replace all
           3.2) apply axiom 1 followed by axiom 2 until no more Less constructors exists inside the chosen temporal operator and mark current until operator as solved

   *)

  let heuristic dnf_map =

    let solve_isolation key (tuple: idx_ct_fm_disj_ex) (unsolved_map_fm, solved_map_fm) =

        (* to isolate until operators *)
        let isol_until_oper pval (fm1: fm_disj_ex) (fm2: fm_disj_ex) : idx_ct_fm_disj_ex =
          (* put fm1 in disjuntive normal form and isolate inequalities *)
          let lst_dnf_nineq,lst_dnf_wineq = isol_disj fm1 in

          if lst_dnf_wineq <> [] then
            (if lst_dnf_nineq <> [] then
              KFormula(apply_axiom axiom1_primitive pval (fm_disj_ex_of_fm_conj_ex_lst lst_dnf_nineq) (lst_dnf_wineq) fm2)
            else
              KFormula(apply_axiom axiom1_primitive pval (`Conj(`X(True))) (lst_dnf_wineq) fm2)
              (* ###### CONFIRM THIS LINE *)
            )
          else
            begin
              (* put fm2 in DNF and isolate inequalities *)
              let lst_dnf_nineq2,lst_dnf_wineq2 = isol_disj fm2 in

              if lst_dnf_wineq2 <> [] then
                (if lst_dnf_nineq2 <> [] then
                  KFormula(apply_axiom axiom2_primitive pval (fm_disj_ex_of_fm_conj_ex_lst lst_dnf_nineq2) (lst_dnf_wineq2) fm1)
                else
                  KFormula(apply_axiom axiom2_primitive pval (`Conj(`X(True))) (lst_dnf_wineq2) fm1)
                  (* ###### CONFIRM THIS LINE *)
                )
              else
                KUntil(pval,fm1,fm2)
            end

        in

        (* to isolate duration terms *)
        (* [TODO] this is unnused *)
        (*let isol_dur_oper t (tm: tm_disj_ex) (fm: fm_disj_ex) : idx_ct_fm_disj_ex =
          KFormula(`Conj(`X(True)))
        in*)

        match tuple with
        | KUntil(pval, fm1, fm2) ->
            (* If all variables in fm1 and fm2 are solved then apply the axiom else skip it for next time *)
            if (is_fm_disj_ex_solved fm1 unsolved_map_fm solved_map_fm) && (is_fm_disj_ex_solved fm2 unsolved_map_fm solved_map_fm) then
            begin
              verb_m 2 (fun _ -> print_endline ( "fm1: "^( Sexp.to_string (sexp_of_fm_disj_ex fm1) )^"\nfm2: "^(Sexp.to_string (sexp_of_fm_disj_ex fm2))^"\n" ); );
              let out_fm = isol_until_oper pval fm1 fm2 (* let us begin by isolating this until operator *)
              in verb_m 2 (fun _ -> print_endline ( "isol_until_oper: "^( Sexp.to_string (sexp_of_idx_ct_fm_disj_ex out_fm) )^"\n" ); );
              let unsolved_fm_x = replace_fm key out_fm (* replace until operators with new propositions and simplify the remaining formula *)
              in if unsolved_fm_x <> Fm_container.empty then
                (join_map_fm unsolved_fm_x unsolved_map_fm, solved_map_fm) (* add new replacements to the unsolved map as well as the remaining formula *)
              else (unsolved_map_fm, Fm_container.add key (out_fm) solved_map_fm)
            end
            else (unsolved_map_fm, solved_map_fm)


        | KDuration(fm, tm) ->  (* duration term case *)
          (* If all variables in fm and tm are solved then apply the axiom for duration isolation else skip it *)
          if (is_fm_disj_ex_solved fm unsolved_map_fm solved_map_fm) && (is_tm_disj_ex_solved tm unsolved_map_fm solved_map_fm) then
            (* TODO HERE *)
            (unsolved_map_fm, solved_map_fm)
          else (unsolved_map_fm, solved_map_fm)


        | KFormula(fm) -> 	(* if formula is solved then mark as solved else continue solving *)
          if is_fm_disj_ex_solved fm unsolved_map_fm solved_map_fm then
            (unsolved_map_fm, Fm_container.add key (KFormula(fm)) solved_map_fm)
          else (Fm_container.add key (KFormula(fm)) unsolved_map_fm, solved_map_fm)

        (* | _ -> raise (Failure ("simplify_heuristic applied to unknown tokens")) *)
    in

    let rec solve depth dnfmap = (* depth: max depth for search *)
      if dnfmap = Fm_container.empty then
        raise (Failure ("simplify_heuristic of an empty formula"))
      else (
          let unsolved_fm, solved_fm = Fm_container.fold solve_isolation dnfmap (Fm_container.empty, Fm_container.empty)
          in
          
          if unsolved_fm = Fm_container.empty then
            solved_fm
          else ( 
          	if depth > 0 then
              solve (depth-1) dnfmap
            else raise (Failure ("heuristic solver exits with unsolved formulas"))
          )
      )
    in

    solve 1000 dnf_map (* max depth fixed to 1000 iterations *)

  in


  try
  (* open  mathematica task to aid simplification *)
  Mathkernel.mk_init ();
  Mathkernel.mk_handshake ();

  (* test if mathematica has been correctly initalized *)
  Mathkernel.mk_writeln "$Version";
  let answer = Mathkernel.mk_readln () in
  if answer <> "" then
    verb_m 1 (fun _ -> print_endline ("Mathematica has been initialized sucessfully with version "^(BatString.strip answer)^".");)
  else raise (Failure ("Mathematica has not been initialized properly."));

  (* type conversion from fm to fm_disj_ex *)
  let map_ex = fm_to_fm_disj_ex_map rmtld_formula in

  verb_m 1 (fun _ -> print_endline ("Type conversion from fm to fm_disj_ex done.\n"); print_fm_map_ex map_ex; );
  
  (* simplify using heuristic *)
  let fm_map_solved = heuristic map_ex in

  verb_m 1 (fun _ -> print_endline ("Simplifcation using heuristic done.\n"); print_fm_map_ex fm_map_solved;);

  (* convert map_fm_disj_ex to fm *)
  (*let retval : fm = fm_of_fm_disj (fm_disj_of_fm_disj_ex (fm_disj_ex_of_map_fm_disj_ex fm_map_solved)) in*)
  let retval : fm = fm_of_map_fm_disj_ex fm_map_solved in

  (* close mathematica process *)
  Mathkernel.close_process_mathematica ();
  
  retval

  with excp -> (* close mathematica process *)
  Mathkernel.close_process_mathematica (); raise excp
