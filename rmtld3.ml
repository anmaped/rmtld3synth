(*pp camlp4o `ocamlfind query type_conv`/pa_type_conv.cma  `ocamlfind query pa_sexp_conv`/pa_sexp_conv.cma  -I `ocamlfind query sexplib` -I `ocamlfind query pa_sexp_conv` *)

(* RESTRICTED METRIC TEMPORAL LOGIC WITH DURATIONS EVALUATION MODULE
 *
 * The RMTLD3 syntax and semantics have been implemented in this module. The
 * evaluation of the logical formulas is given by the models function, and the
 * generation of RMTLD formulas is given by gen_formula function.
 *
 * *)

open Batteries
open List

open Sexplib
open Sexplib.Conv


type var_id = string with sexp
type prop   = string with sexp
type time   = float with sexp
type value  = float with sexp

type formula =
          True of unit
        | Prop of prop
        | Not of formula
        | Or of formula * formula
        | Until of time * formula * formula
        | Exists of var_id * formula
        | LessThan of term * term
and term =
          Constant of value
        | Variable of var_id
        | FPlus of term * term
        | FTimes of term * term
        | Duration of term * formula
with sexp

type rmtld3_fm = formula with sexp
type rmtld3_tm = term with sexp
type tm = rmtld3_tm with sexp
type fm = rmtld3_fm with sexp
(*type foobar = Foo of int | Bar with sexp*)


type term_indefeasible = V of value | Indefeasible

(* three valued logic *)
type three_valued = True | False | Unknown
(* special type for until definition *)
type three_valued_symbol = STrue | SFalse | SUnknown | Symbol

(* type conversion *)
let b3_to_b4 b3 = if b3 = True then STrue else (if b3 = False then SFalse else SUnknown)

(* convert three_valued_symbol into three_valued type *)
let b4_to_b3 b4 = if b4 = STrue then True else ( if b4 = SFalse then False else Unknown )

(* boolean into three_valued type *)
let b_to_b3 b = if b=true then True else False

(* three_valued_symbol into string type *)
let b4_to_string b4 = if b4 = STrue then "true" else if b4= SFalse then "false"
else if b4 = SUnknown then "unknown" else "symbol"

(* three_valued into string type *)
let b3_to_string b3 = if b3 = True then "true" else if b3= False then "false"
else "unknown"

(* Boolean operators *)
(* OR *)
let b3_or b31 b32 =
  if b31 = True || b32 = True then
    True
  else (
    if b31 = False && b32 = False then
      False
    else
        Unknown
  )
(* NOT *)
let b3_not b3 = if b3 = True then False else if b3=False then True else Unknown

(* Relation operator < *)
let b3_lessthan n1 n2 = if n1 < n2 then True else (if n1 >= n2 then False else Unknown)


(*
 *  trace is a list of n elements of the form,
 *    (prop, interval_1),...,(prop, interval_n)
 *)
type trace = (prop * (time*time)) list

(* environment record type *)
type ev =
  {
    trace             : trace * trace ; (* inverted trace and normal tail trace *)
    duration_of_trace : time ;
    trace_cardinality : int ;
    evaluate          : trace * trace -> prop -> time -> three_valued ;
  }

(* Logic observation/environment record type *)
type logic_obs =
  {
    mutable theta : (var_id * value) list ;
    eval          : var_id -> value ;
    add           : var_id -> value -> unit ;
    remove        : var_id -> unit ;
  }

let activate_debug = ref false

let count = ref 0
let count_duration = ref 0

(* Generating RMTLD formulas *)
(* Generation Specifications:
 *
 * For Constant we use a normal distribution to select values (10,3)
 * For Variable name we use letters of alphabet
 * For Integral Value we use the exponential distribution
 * 
 * For Prop name we use the alphabet letters
 * For Until Value we use the exponential distribution
 * For Exist name we use the alphabet letters
 *
 * To chose a formula we apply the uniform distribution from 1 to 6
 * To chose a term we apply the uniform distribution from 1 to 3
 *)

(* Flag for quantifier generation *)
let gen_quantifiers = ref true

(* gen_formula function *)
let rec gen_term size p =
   if size > 0 then
   match Random.int (if gen_quantifiers = ref true then 3 else 2) with (* remove existential generation *)
     | 0 -> Constant(Random.float p)
     | 1 -> Duration(gen_term (size-1) p, gen_formula (size-1) p)
     | 2 -> Variable(
                     match Random.int 2 with
                     | 0 -> "va"
                     | 1 -> "vb"
                     | _ -> "vc"
                    )
     | _ -> Constant(0.)
   else
     Constant(Random.float p)
and gen_formula size p =
   if size > 0 then
     (* Get sample for formula *)
     match Random.int (if gen_quantifiers = ref true then 6 else 5) with
     | 0 -> Prop(
                        match Random.int 2 with
                        | 0 -> "A"
                        | 1 -> "B"
                        | _ -> "C"
                       )
     | 1 -> Not (gen_formula (size-1) p)
     | 2 -> Or (gen_formula (size-1) p, gen_formula (size-1) p)
     | 3 -> Until (Random.float p, gen_formula (size-1) p, gen_formula (size-1) p)
     | 4 -> LessThan (gen_term size p, gen_term size p)
     | 5 -> Exists (
                    (
                      match Random.int 2 with
                      | 0 -> "va"
                      | 1 -> "vb"
                      | _ -> "vc"
                    ), gen_formula (size-1) p
                   )
     | _ -> Prop("exceed")
   else
     Prop("E")


(*
   Functions for getting results about the search space
*)

(* measuring formulas (n durations, n temporal operators) *)
let rec measure_term term =
   match term with
   | Constant value      -> (0,0)

   | Variable id         -> (0,0)

   | Duration (trm,sf)   -> let x1,y1 = measure_term trm in
                            let x2,y2 = measure_formula sf in
                            (1+x1+x2, y1+y2)

   | FPlus (eta1,eta2)   -> let x1,y1 = measure_term eta1 in
                            let x2,y2 = measure_term eta2 in
                            (x1+x2, y1+y2)

   | FTimes (eta1,eta2)  -> let x1,y1 = measure_term eta1 in
                            let x2,y2 = measure_term eta2 in
                            (x1+x2, y1+y2) 
                            
and measure_formula formula =
   match formula with
   | Prop p          -> (0,0) 
   | Not sf                 -> measure_formula sf
   | Or (sf1, sf2)          -> let x1,y1 = measure_formula sf1 in
                               let x2,y2 = measure_formula sf2 in
                               (x1+x2, y1+y2)
   | Until (pval, sf1, sf2) -> let x1,y1 = measure_formula sf1 in
                               let x2,y2 = measure_formula sf2 in
                               (x1+x2, 1+y1+y2)
   | Exists (var,sf)        -> measure_formula sf
   | LessThan (tr1,tr2)     -> let x1,y1 = measure_term tr1 in
                               let x2,y2 = measure_term tr2 in
                               (x1+x2, y1+y2)

(* create an uniform trace of the type
 * (p1, (i1, i1')), ... ,(pn, (in, in')) list 
 *)
let rec generate_uniform_traces value samples lst =
   if (List.length lst) > samples then lst else
   let timestamp = (Random.float 0.01) +. value in
   match Random.int 2 with
   | 0 -> generate_uniform_traces timestamp samples (("A",(value,timestamp))::lst)
   | 1 -> generate_uniform_traces timestamp samples (("B",(value,timestamp))::lst)
   | _ -> generate_uniform_traces timestamp samples (("C",(value,timestamp))::lst)

(* asymptotic function for complexity *)
let rec asym_comp (a,b,c) fm =
  match fm with
  | Until(vl, fm1, fm2) when a <> []-> fst (fold_left (fun (cts,lst) el -> (cts + (asym_comp (lst,b,c) fm1) + (asym_comp (lst,b,c) fm2), List.tl lst ) ) (0,a) a)
  | Prop(str) -> 1
  | _ -> raise (Failure ("Not supported formula."))

(* generate an until formula using triangle pattern *)
let rec gen_u_formula_with_triangle_pattern rl size p =
  if size > 0 then
    Until((p), gen_u_formula_with_triangle_pattern true (size-1) p, gen_u_formula_with_triangle_pattern false (size-1) p)
  else
    if rl then
      Until((p), Prop("A"), Prop("B"))
    else
      Until((p), Prop("A"), Prop("*"))

(* generate an until formula with maximum likelihood *)
let gen_u_formula_with_maximum_prop_evaluation size pval samples =
  let trc = generate_uniform_traces 0.2 samples []
  in
  let rec gen_u_formula_with_maximum_prop_evaluation' size_op pval trc =
    (*print_endline ("D.. "^(string_of_int size_op));*)
      
    if not (size_op > 0) then
      Prop("A")
    else
      (* test if until of trees is better than the line of trees *)
      let fm1 = gen_u_formula_with_maximum_prop_evaluation' (size_op - 1) pval trc
      in
      let fm2 = gen_u_formula_with_maximum_prop_evaluation' (size_op-2) pval trc
      in
      let n_du1, n_to1 = measure_formula fm1
      in
      let n_du2, n_to2 = measure_formula fm2
      in
      let fm3 = gen_u_formula_with_maximum_prop_evaluation' (size_op - n_to2 - 2) pval trc
      in
      let n_du2, n_to3 = measure_formula fm3
      in
      
      let fm1_tree = Until(pval, fm1, gen_u_formula_with_maximum_prop_evaluation' (size_op - n_to1 - 1) pval trc)
      in
      let fm2_line = Until(pval, Until(pval, fm2, gen_u_formula_with_maximum_prop_evaluation' (size_op - n_to2 - 2) pval trc), gen_u_formula_with_maximum_prop_evaluation' (size_op - n_to2 - n_to3 - 2) pval trc)
      in
      if asym_comp (trc, 0., 0.) fm1_tree < asym_comp (trc, 0., 0.) fm2_line && (size_op - n_to2 - n_to3 - 2) >= 0 then
        fm2_line
      else fm1_tree

  in
  gen_u_formula_with_maximum_prop_evaluation' size pval trc





(* 
 *  Print functions :
 *   - for formulas and terms
 *     * plaintext
 *     * latex
 *   - for traces
 *)

(* print trace *)
let rec print_trace trace =
        if length trace < 1 then () else
        let x,(y,z) = hd trace in
        Printf.printf "(%s,(%f,%f)), " x y z ;
        print_trace (tl trace)

(* convert rmtld formulas to latex language *)
let rec slatex_of_rmtld_tm term =
   match term with
   | Constant value      -> (string_of_float value) ^ " "
   | Variable id         -> id ^ " "
   | Duration (trm,sf)   -> "\\int^{" ^ (slatex_of_rmtld_tm trm) ^ "} \\left(" ^ (slatex_of_rmtld_fm sf) ^ "\\right) "
   | FPlus (eta1,eta2)   -> "\\left( " ^ (slatex_of_rmtld_tm eta1) ^ " + " ^ (slatex_of_rmtld_tm eta2) ^ "\\right)"
   | FTimes (eta1,eta2)  -> "\\left( " ^ (slatex_of_rmtld_tm eta1) ^ " * " ^ (slatex_of_rmtld_tm eta2) ^ "\\right)"
                            
and slatex_of_rmtld_fm formula =
   match formula with
   | Prop p                 -> p ^ " "
   | Not sf                 -> "\\neg \\left(" ^ (slatex_of_rmtld_fm sf) ^ "\\right) "
   | Or (sf1, sf2)          -> "\\left(" ^ (slatex_of_rmtld_fm sf1) ^ "\\lor " ^ (slatex_of_rmtld_fm sf2) ^ "\\right)"
   | Until (pval, sf1, sf2) -> "\\left(" ^ slatex_of_rmtld_fm sf1 ^ "\\ U_{" ^ (string_of_float pval) ^ "} \\ " ^ (slatex_of_rmtld_fm sf2) ^ "\\right)"
   | Exists (var,sf)        -> "\\exists " ^ var ^ " \\ \\left(" ^ (slatex_of_rmtld_fm sf) ^ "\\right)"
   | LessThan (tr1,tr2)     -> "\\left(" ^ (slatex_of_rmtld_tm tr1) ^ "< " ^ (slatex_of_rmtld_tm tr2) ^ "\\right)"

(* Print formulas and terms for latex *)
let print_latex_formula f = print_endline (slatex_of_rmtld_fm f)


(* convert rmtld formulas to plain text *)
let rec string_of_rmtld_tm rmtld_tm = 
  match rmtld_tm with
  | Constant value      -> (string_of_float value) ^ " "
  | Variable id         -> id ^ " "
  | Duration (trm,sf)   -> "int[" ^ (string_of_rmtld_tm trm) ^ "] (" ^ (string_of_rmtld_fm sf) ^ ") "
  | FPlus (eta1,eta2)   -> "( " ^ (string_of_rmtld_tm eta1) ^ " + " ^ (string_of_rmtld_tm eta2) ^ ")"
  | FTimes (eta1,eta2)  -> "( " ^ (string_of_rmtld_tm eta1) ^ " * " ^ (string_of_rmtld_tm eta2) ^ ")"
   
and string_of_rmtld_fm rmtld_fm =
  match rmtld_fm with
  | True()                 -> "true"
  | Prop p                 -> p
  | Not sf                 -> "~(" ^ (string_of_rmtld_fm sf) ^ ") "
  | Or (sf1, sf2)          -> "(" ^ (string_of_rmtld_fm sf1) ^ " or " ^ (string_of_rmtld_fm sf2) ^ ")"
  | Until (pval, sf1, sf2) -> "(" ^ (string_of_rmtld_fm sf1) ^ " U_" ^ (string_of_float pval) ^ " " ^ (string_of_rmtld_fm sf2) ^ ")"
  | Exists (var,sf)        -> "exists " ^ var ^ " (" ^ (string_of_rmtld_fm sf) ^ ")"
  | LessThan (tr1,tr2)     -> "(" ^ (string_of_rmtld_tm tr1) ^ " < " ^ (string_of_rmtld_tm tr2) ^ ")"

(* print formulas and terms in plaintext *)
let print_plaintext_formula f = print_endline (string_of_rmtld_fm f)


(* Convert a trace into an observation set *)
let observation duration (trace_backward,trace_forward) p t =
  let rec search trace prop t =
    let (b_p, (b_i, b_i')) = hd trace in
    (* check one element *)
    if (b_i <= t && t < b_i') then
      if prop = b_p then True else False
    else
      (* search next *)
      search (tl trace) prop t
  in
  let search_forward trace prop t =
    let (b_p, (b_i, b_i')) = hd trace in
    (* check t lower b_i *)
    if (not (b_i <= t)) then
      False
    else
      search trace prop t
  in
  let search_backward trace prop t =
    let (b_p, (b_i, b_i')) = hd trace in
    (* check t upper b_i' *)
    if (not (t < b_i')) then
      False
    else
      search trace prop t
  in
  (*Printf.fprintf stdout "%i %i\n " (length trace_backward) (length trace_forward);
  Printf.printf " S--- ";
  print_trace trace_backward;
  Printf.printf "--- \n ";
  print_trace trace_forward;*)  
  if ((length trace_forward < 1) && (length trace_backward < 1))  || t >= duration then
    Unknown
  else (
    if (length trace_backward < 1) && not (length trace_forward < 1)  then
      (* check in one direction *)
       search_forward trace_forward p t
    else (
      if not (length trace_backward < 1) && (length trace_forward < 1) then
        (* check in one direction *)
         search_backward trace_backward p t
      else (
        (* go both ways the way to go *)
        b3_or (search_backward trace_backward p t) (search_forward trace_forward p t)
      )
    )
  )

(* Environment record instantiation *)
let environment trace =
  let duration = (function [] -> 0. | x::xs -> fold_left
    (fun a (_,(x1,x2)) -> a +. (x2-.x1)) 0. (x::xs)) trace in
  let trace_tuple = ([],trace) in
    {
      trace = trace_tuple ;
      duration_of_trace =  duration;
      evaluate = observation duration ;
      trace_cardinality = length trace ;
    }

(* Logical environment instantiation *)
(* unused since quantified formulas are previously simplified *)
let rec logical_environment = {
	theta = []; (* Example of the environment: [(a,10); (b,20)]*)
	eval = (fun var -> assoc var logical_environment.theta );
	add = (fun var value ->  logical_environment.theta <- (var,value)::logical_environment.theta; () );
	remove = (fun var -> logical_environment.theta <- remove_assoc var logical_environment.theta; () );
}

(* sub-trace function *)
let sub_k (k,u,t) gamma =
       (* construct a sub list *)
       let _,tb = k.trace in
       (* check k size *)
       if length tb < 1 then
         []
       else
         let p1,p2 = partition (fun (_,(i1,i2)) -> if t <= i1 && i1 < (t+.gamma) then true else false) tb in
         (*Printf.printf "(%f)s-" t ;
         print_trace p1;
         Printf.printf "--" ;
         print_trace p2;
         Printf.printf "-e";*)
         p1

(*
(* sub-trace function *)
let sub_k (k,u,t) gamma =
  if k.duration_of_trace <= (t +. gamma) then
    (* possible unknown value *)
  else
    (* only true or false *)
*)

(* next trace function *)
(*let next_k k =
        let tb,tf = k.trace in
        (* check k size *)
        if length tf < 1 then raise (Failure "next_k k=(_,[])") else
        {
          trace = (hd tf)::tb, (tl tf) ;
          duration_of_trace =  k.duration_of_trace ;
          evaluate = k.evaluate ;
          trace_cardinality = k.trace_cardinality ;
        }
*)
(* previous trace function *)
(*let prev_k k =
        let tb,tf = k.trace in
        (* check k size *)
        if length tb < 1 then raise (Failure "prev_k k=([],_)") else
        {
          trace = (tl tb), (hd tb)::tf ;
          duration_of_trace =  k.duration_of_trace ;
          evaluate = k.evaluate ;
          trace_cardinality = k.trace_cardinality ;
        }
*)

(* Compute function has the following inputs. The environment, the logical environment,
 * the initial time, and the MTLD formula. This function has the following
 * meaning.
 *  [RMTLD3 formula](env, lg_env, t) in {True,False,Unknown}
 *)
let rec compute_term m t term =
	match term with
	| Constant value       -> value
	| Duration (di,phi)    -> compute_term_duration m (t, (compute_term m t  di)) phi
	| FPlus (tr1,tr2)      -> compute_term m t tr1 +. compute_term m t tr2
  | FTimes (tr1,tr2)     -> compute_term m t tr1 *. compute_term m t tr2
	| _ -> raise (Failure "compute_terms: missing term")

and compute_term_duration (k,u) dt formula =
        let indicator_function (k,u) t phi = if compute (k,u,t) phi = True then 1. else 0. in
        let riemann_sum m dt (i,i') phi =
          (* dt=(t,t') and t in ]i,i'] or t' in ]i,i'] *)
          count_duration := !count_duration + 1 ;
          let t,t' = dt in
          if i <= t && t < i' then
            (* lower bound *)
            (i'-.t) *. (indicator_function m t phi)
          else (
            if i <= t' && t' < i' then
              (* upper bound *)
              (t'-.i) *. (indicator_function m t' phi)
            else
              (i'-.i) *. (indicator_function m i phi)
          ) in
        let eval_eta m dt phi x = fold_left (fun s (prop,(i,t')) -> (riemann_sum
        m dt (i,t') phi) +. s) 0. x in
        let t,t' = dt in
        eval_eta (k,u) dt formula (sub_k (k,u,t) t')

and compute (env, lg_env, t) formula =
        match formula with
        | True()                  -> True
        | Prop p                  ->
          (* counting proposition evaluation instead of recursive calls *)
          count := !count + 1 ;
          env.evaluate env.trace p t
        | Not sf                  -> b3_not (compute (env, lg_env, t) sf)
        | Or (sf1, sf2)           -> b3_or (compute (env, lg_env, t) sf1) (compute (env, lg_env, t) sf2)
        | Until (gamma, sf1, sf2) -> compute_uless (env, lg_env, t) gamma sf1 sf2
        | LessThan (tr1,tr2)      -> b3_lessthan (compute_term (env, lg_env) t tr1)
                                       (compute_term (env, lg_env) t tr2)
        | _                       -> raise (Failure ("compute: bad formula "^( Sexp.to_string_hum (sexp_of_rmtld3_fm formula))))

and compute_uless m gamma phi1 phi2 =

       let eval_i b1 b2 = if b2 <> False then
                            b3_to_b4 b2
                           else if b1 <> True && b2 = False then
                                  b3_to_b4 b1
                                else
                                  Symbol in
        let eval_b m phi1 phi2 v = if v = Symbol then
          begin
            let cmpphi1 = compute m phi1 in
            let cmpphi2 = compute m phi2 in
            if activate_debug = ref true then
            begin
              Printf.printf "SY: phi1: %s | phi2: %s\n" (b3_to_string (cmpphi1)) (b3_to_string (cmpphi2)) ;
            end;
            
            let rs = eval_i (cmpphi1) (cmpphi2) in
            
            if activate_debug = ref true then
            begin
              Printf.printf "SY: result %s\n" (b4_to_string rs) ;
            end;
            
            rs
          end
          else v in
        let eval_fold (k,u,t) phi1 phi2 x =
          if activate_debug = ref true then
          begin
            print_trace x;
            Printf.printf "\nfold_init\nFormula1: ";
            print_plaintext_formula phi1 ;
            Printf.printf "\nFormula2: " ;
            print_plaintext_formula phi2 ;
            Printf.printf "\n" ;
          end;
          let s,_ = fold_left (fun (v,t') (prop,(ii1,ii2)) ->

            if activate_debug = ref true then
            begin
              Printf.printf "[%f, %f[ ** ThruthValue: %s \n" ii1 ii2 (b4_to_string v);
            end;

            (eval_b (k, u, t') phi1 phi2 v, ii2)) (Symbol,t) x in
            
            if activate_debug = ref true then
            begin
              Printf.printf "fold_end : %s \n" (b4_to_string s) ;
            end;

            s in
        if gamma >= 0. then
        begin
          if activate_debug = ref true then
          begin
            Printf.printf "BeginULess \n";
          end ;
          let k,_,t = m in
          let subk = sub_k m gamma in
          let eval_c = eval_fold m phi1 phi2 subk in
          if eval_c = Symbol then
            (* we have two cases to consider *)
            (* when the time bound is the last symbol return False *)
            (* when the time bound is greater than trace return Unknown *)
            if k.duration_of_trace <= (t +. gamma) then
              Unknown
            else (
              False
            )
          else
            let rs = b4_to_b3 eval_c in
            if activate_debug = ref true then
            begin
              Printf.printf "EndULess: %s\n" (b3_to_string rs) ;
            end ;
            rs
        end
        else
          (* failure: gamma is not a non-negative value *)
          raise  (Failure "Gamma of U operator is a non-negative value")



(* compute the temporal upper bound of a formula *)
let rec calculate_t_upper_bound (formula: rmtld3_fm) =
  match formula with
    | True()                  -> 0.
    | Prop p                  -> 0.
    | Not sf                  -> calculate_t_upper_bound sf
    | Or (sf1, sf2)           -> Pervasives.max (calculate_t_upper_bound sf1) (calculate_t_upper_bound sf2)
    | Until (gamma, sf1, sf2) -> gamma +. Pervasives.max (calculate_t_upper_bound sf1) (calculate_t_upper_bound sf2)
    | LessThan (tr1,tr2)      -> Pervasives.max (calculate_t_upper_bound_term tr1) (calculate_t_upper_bound_term tr2)
    | _ -> raise (Failure "ERROR: Calculating bound for unsupported formula.") 
and calculate_t_upper_bound_term term =
  match term with
    | Constant value       -> 0.
    | Duration (di,phi)    -> 0.
    | FPlus (tr1,tr2)      -> 0.
    | FTimes (tr1,tr2)     -> 0.
    | _ -> raise (Failure "ERROR: Calculating bound for unsupported term.")


(* RMTLD3 abreviations *)
let mtrue : formula = True() (*Or(Prop "NOSYMBOL", Not(Prop "NOSYMBOL"))*)
let mfalse = Not(mtrue)
let mand phi1 phi2 = Not (Or (Not phi1, Not phi2))
let mimplies phi1 phi2 = Or (Not (phi1), phi2)
let meventually t phi = Until (t, mtrue, phi)
let malways t phi = Not (meventually t (Not phi))
let forall var phi = Not(Exists(var, Not(phi)))

let greater tm1 tm2 = LessThan(tm2, tm1)
let not_equal tm1 tm2 = Or(LessThan(tm1, tm2), LessThan(tm2,tm1))
let equal tm1 tm2 = Not(not_equal tm1 tm2)
let less_or_equal tm1 tm2 = Or(LessThan(tm1, tm2), equal tm1 tm2)
let greater_or_equal tm1 tm2 = Or(greater tm1 tm2, equal tm1 tm2)



(* CONTINUE HERE !!! *)

(* shorthand for simple duration inequalities *)
let m_duration_less cons1 formula cons2 = LessThan(Duration(cons1, formula), cons2)
let m_duration_less2 cons2 cons1 formula = LessThan(cons2, Duration(cons1, formula))
let m_duration_notequal cons1 formula cons2 = Or((m_duration_less cons1 formula cons2), (m_duration_less2 cons2 cons1 formula))
let m_duration_equal cons1 formula cons2 = Not(m_duration_notequal cons1 formula cons2)
let m_duration_lessorequal cons1 formula cons2 = Or(m_duration_less cons1 formula cons2, m_duration_equal cons1 formula cons2)

let m_duration_notequal2 cons2 cons1 formula = Or((m_duration_less2 cons2 cons1 formula), (m_duration_less2 cons1 cons2 formula))
let m_duration_equal2 cons2 cons1 formula = Not(m_duration_notequal2 cons2 cons1 formula)
let m_duration_lessorequal2 cons2 cons1 formula = Or(m_duration_less2 cons2 cons1 formula, m_duration_equal2 cons2 cons1 formula)


let _ =
  Random.self_init ();
  (*Printf.printf "Random Seed Initialized !\n";*)
  
  activate_debug := false;
  let activate_graph_generation = ref false in

  if activate_graph_generation = ref true then
  begin
    (*print_latex_formula formula;*)

    (* time bins *)
    (*let bins = [(0.,0.01); (0.01,0.1); (0.1,0.2); (0.2,0.6); (0.6,0.8); (0.8,1.);
    (1.0,1.5); (1.5,2.); (2.,3.); (3.,5.)] in*)
    
    (* count bins *)
    let bins = [(0.,10.); (10.,100.); (100.,1000.); (1000.,10000.); (10000.,100000.); (100000.,1000000.); (1000000.,10000000.);] in

    let bins_array = Array.of_list bins in
    let bin_first,_ = hd bins in
    let _,bin_last = hd (rev bins) in
    let n_bins = length bins in
    let metrics_array = Array.make n_bins [] in
    
    let metrics (delta_t, count, n_to,n_du) =
      let v = count in
      (* create bins *)
      if bin_first >= v then
         metrics_array.(0) <- (delta_t, n_to, n_du)::(metrics_array.(0))
      else (
        if v > bin_last then
          metrics_array.(n_bins-1) <- (delta_t, n_to, n_du)::(metrics_array.(n_bins-1))
        else
          (* calculate bin to insert *)
          for i=0 to n_bins-1 do
            let (b,b') = bins_array.(i) in
            if b <= v && v < b' then
              metrics_array.(i) <- (delta_t, n_to, n_du)::(metrics_array.(i)) ;
          done;   
          ()
      ) in

    let fprint_metrics_list oc lst =
      fold_left (fun a (x,y,z) -> Printf.fprintf oc "%f %i %i\\\\ " x y z  ) () lst ;
      () in

    let fprint_metrics oc =
      (* print bin by line *)
      for i=0 to n_bins-1 do
        Printf.fprintf oc "\\addplot[boxplot] table[row sep=\\\\,y index=0] {" ;
        fprint_metrics_list oc (metrics_array.(i)) ;
        Printf.fprintf oc "};\n"
      done;
      ()
    in

    let rec strategic_uniform_trace value samples factor trace =
     let trace_size =  length trace in
     (*let timestamp = (Random.float factor) +. value in*)
     let timestamp = factor +. value in
     if samples = 0 then
       (("B",(value,timestamp))::trace)
     else
       if samples <= trace_size then
         strategic_uniform_trace timestamp (samples-1) factor (("B",(value,timestamp))::trace)
       else
         strategic_uniform_trace timestamp (samples-1) factor (("A",(value,timestamp))::trace)
    in

     
    
    (* until case ... *)
    (* until(_,until(...),until(...)) *)
    let linear_plot_file = open_out "linear_plot.tex" in
    let ha = Array.of_list [1; 2; 3; 5; 10; 15;] in
    (*let ha = Array.of_list [1; 2; 3; 4; 5; 6; 7; 8; 9; 10;] in*)
    (*let matrix_data = Array.of_list [(10, ha); (100, ha); (1000, ha); (10000, ha); (100000, ha);] in*)
    (*let matrix_data = Array.of_list [(10, ha); (100, ha); (1000, ha);] in*)
    let matrix_data = Array.of_list [(10, ha); (100, ha); (1000, ha);] in
    for st=0 to ((Array.length matrix_data)-1) do
      let sizeof_trace, mheight = matrix_data.(st) in
      Printf.fprintf linear_plot_file "\\addplot[color=black,mark=o,solid,] table[row sep=\\\\,y index=2, x index = 1]  {" ;
      for j=0 to ((Array.length mheight)-1) do
        let height = mheight.(j) in
        (* construct trace and formula *)
        (*let trace = rev (generate_uniform_traces 0. sizeof_trace []) in*)
        (* strategic trace construction *)
        (* factor * sizeof_trace * 2 = duration -> factor = duration / (sizeof_trace*2) *)
        let duration = 10. in
        let trace = rev (strategic_uniform_trace 0. sizeof_trace (duration /. ( (float_of_int sizeof_trace) /. 1.95)) []) in
        let formula = gen_u_formula_with_triangle_pattern true (height-2) duration in
        (*((2. ** (float_of_int (height)))/. (float_of_int sizeof_trace)) *)
        (*print_trace trace ;*)
        Printf.printf "\nFormula with height=%i and |trace|=%i: \n  " height sizeof_trace ;
        (*print_plaintext_formula formula;*)
        Printf.printf "\n %!" ;
        let env = environment trace in (* generate environment based on trace *)
        let lg_env = logical_environment in (* logic environment -- not used when gen_quantifiers is false *)

        (* reset count *)
        count := 0;

        let time_start = Sys.time () in
        let _ev = compute (env, lg_env, 0.) formula in
        let time_end = Sys.time () in
        let delta_t = (time_end -. time_start) in
        Printf.fprintf linear_plot_file "%i %f %i %i %f\\\\ %!" !count delta_t height sizeof_trace (2. ** (float_of_int height)) ;
        ()
      done;
        Printf.fprintf linear_plot_file "};\n" ;
    done;
    
    close_out linear_plot_file ;


    let oc = open_out "data.tex" in
    let nsamples = 1000 in
    let sizeof_trace = 100 in
    (* Calculate the execution time of the formula with height 5 *)
    let height = 5 in
    gen_quantifiers := false; (* formulas without quantifiers *)
    for i=1 to nsamples do
       let formula = gen_formula height ((2. ** (float_of_int (height)))/. (float_of_int sizeof_trace)) in (* let formula = gen_formula height in *)
       (* generate uniform traces *)
       let trace = rev (generate_uniform_traces 0. sizeof_trace []) in
       (*print_trace trace ;*)
       print_plaintext_formula formula;
       let env = environment trace in (* generate environment based on trace *)
       let lg_env = logical_environment in (* logic environment -- not used when gen_quantifiers is false *)
       
       (* reset count *)
       count := 0;
       count_duration := 0;

       let time_start = Sys.time () in
       let _ev = compute (env, lg_env, 0.) formula in
       let time_end = Sys.time () in
       let n_du, n_to = measure_formula formula in
       let delta_t = (time_end -. time_start) in 
       Printf.printf "Duration %fs\nMetrics:\n" delta_t;
       Printf.printf "  Cardinality: %d\n" (length trace);
       Printf.printf "  Measure(temporal operators): %d\n" n_to;
       Printf.printf "  Measure(duration terms): %d\n" n_du;
       Printf.printf "  Count: %d\n" !count;
       (*Printf.fprintf oc "%f; %d; %d\n" delta_t n_to n_du ;*)
       metrics (delta_t, float_of_int (!count + !count_duration), n_to, n_du) ;
       ()
    done;

    fprint_metrics oc ;
    
    close_out oc;
    
  end;


  

  ()
;;

