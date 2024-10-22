(* 
 * Three-valued Restricted Metric Temporal Logic with Durations Core Module
 *
 * The RMTLD3 syntax and semantics have been implemented in this module. The
 * evaluation of the logical formulas is given by the models function, and the
 * generation of RMTLD formulas is given by gen_formula function.
 *
 *)

open List
open Sexplib
open Sexplib.Std
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type var_id = string [@@deriving sexp, yojson]

type prop = string [@@deriving sexp, yojson]

type time = float [@@deriving sexp, yojson]

type value = float [@@deriving sexp, yojson]

type fm =
  | True of unit
  | Prop of prop
  | Not of fm
  | Or of fm * fm
  | Until of time * fm * fm
  | Since of time * fm * fm
  | Exists of var_id * fm
  | LessThan of tm * tm
  | Until_eq of time * fm * fm
  | Since_eq of time * fm * fm

and tm =
  | Constant of value
  | Variable of var_id
  | FPlus of tm * tm
  | FTimes of tm * tm
  | Duration of tm * fm
[@@deriving sexp]

type rmtld3_tm = tm [@@deriving sexp]

type rmtld3_fm = fm [@@deriving sexp]

(* shorthands for RMTLD3 formulas *)
let mtrue : rmtld3_fm = True ()

let mfalse = Not mtrue

let mand phi1 phi2 = Not (Or (Not phi1, Not phi2))

let mand_list lst = List.fold_left (fun a b -> mand a b) (hd lst) (tl lst)

let mimplies phi1 phi2 = Or (Not phi1, phi2)

let until_leq t phi1 phi2 =
  Or (Until_eq (t, phi1, phi2), Until (t, phi1, phi2))

let since_leq t phi1 phi2 =
  Or (Since_eq (t, phi1, phi2), Since (t, phi1, phi2))

let meventually t phi = Until (t, mtrue, phi)

let meventually_eq t phi = Until_eq (t, mtrue, phi)

let meventually_leq t phi = until_leq t mtrue phi

let mpasteventually t phi = Since (t, mtrue, phi)

let mpasteventually_eq t phi = Since_eq (t, mtrue, phi)

let mpasteventually_leq t phi = since_leq t mtrue phi

let malways t phi = Not (meventually t (Not phi))

let malways_eq t phi = Not (meventually_eq t (Not phi))

let malways_leq t phi = Not (meventually_leq t (Not phi))

let mhistorically t phi = Not (mpasteventually t (Not phi))

let mhistorically_eq t phi = Not (mpasteventually_eq t (Not phi))

let mhistorically_leq t phi = Not (mpasteventually_leq t (Not phi))

let mnext t phi = Until (t, mfalse, phi)

let mnext_eq t phi = Until_eq (t, mfalse, phi)

let mnext_leq t phi = until_leq t mfalse phi

let mprev t phi = Since (t, mfalse, phi)

let mprev_eq t phi = Since_eq (t, mfalse, phi)

let mprev_leq t phi = since_leq t mfalse phi

let forall var phi = Not (Exists (var, Not phi))

let greater tm1 tm2 = LessThan (tm2, tm1)

let not_equal tm1 tm2 = Or (LessThan (tm1, tm2), LessThan (tm2, tm1))

let equal tm1 tm2 = Not (not_equal tm1 tm2)

let less_or_equal tm1 tm2 = Or (LessThan (tm1, tm2), equal tm1 tm2)

let greater_or_equal tm1 tm2 = Or (greater tm1 tm2, equal tm1 tm2)

(* shorthand for simple duration inequalities *)
let m_duration_less cons1 formula cons2 =
  LessThan (Duration (cons1, formula), cons2)

let m_duration_less2 cons2 cons1 formula =
  LessThan (cons2, Duration (cons1, formula))

let m_duration_notequal cons1 formula cons2 =
  Or
    ( m_duration_less cons1 formula cons2
    , m_duration_less2 cons2 cons1 formula )

let m_duration_equal cons1 formula cons2 =
  Not (m_duration_notequal cons1 formula cons2)

let m_duration_lessorequal cons1 formula cons2 =
  Or
    ( m_duration_less cons1 formula cons2
    , m_duration_equal cons1 formula cons2 )

let m_duration_notequal2 cons2 cons1 formula =
  Or
    ( m_duration_less2 cons2 cons1 formula
    , m_duration_less2 cons1 cons2 formula )

let m_duration_equal2 cons2 cons1 formula =
  Not (m_duration_notequal2 cons2 cons1 formula)

let m_duration_lessorequal2 cons2 cons1 formula =
  Or
    ( m_duration_less2 cons2 cons1 formula
    , m_duration_equal2 cons2 cons1 formula )

(* rmtld3 index type for fm type container *)
type idx_ct =
  | KUntil of time * fm * fm
  | KDuration of fm * tm
  | KFormula of fm

(* Untimed trace is a sequence of the form: prop1, prop2, ... *)
type trace_untimed = prop list [@@deriving sexp]

(*
 *  trace is an associate list with n elements of the form
 *    (prop, duration_1), ..., (prop, duration_n)
 *)
type trace = (prop * time) list [@@deriving sexp, yojson]

type term_indefeasible = V of value | Indefeasible

(* three valued logic *)
type three_valued = True | False | Unknown

(* special type for until definition *)
type three_valued_symbol = STrue | SFalse | SUnknown | Symbol

type duration = Dnone | Dsome of float

(* type conversion *)
let b3_to_b4 b3 =
  if b3 = True then STrue else if b3 = False then SFalse else SUnknown

(* convert three_valued_symbol into three_valued type *)
let b4_to_b3 b4 =
  if b4 = STrue then True else if b4 = SFalse then False else Unknown

(* boolean into three_valued type *)
let b_to_b3 b = if b = true then True else False

(* three_valued_symbol into string type *)
let b4_to_string b4 =
  if b4 = STrue then "true"
  else if b4 = SFalse then "false"
  else if b4 = SUnknown then "unknown"
  else "symbol"

(* three_valued into string type *)
let b3_to_string b3 =
  if b3 = True then "true" else if b3 = False then "false" else "unknown"

(* Boolean operators *)
(* OR *)
let b3_or b31 b32 =
  if b31 = True || b32 = True then True
  else if b31 = False && b32 = False then False
  else Unknown

(* NOT *)
let b3_not b3 =
  if b3 = True then False else if b3 = False then True else Unknown

(* Relation operator < *)
let b3_lessthan n1 n2 =
  match (n1, n2) with
  | Dnone, Dnone | Dsome _, Dnone | Dnone, Dsome _ -> Unknown
  | Dsome v1, Dsome v2 -> if v1 < v2 then True else False
(*if n1 < n2 then True else if n1 >= n2 then False else Unknown*)

(* observation type (propositional part) *)
type env = {trc: trace; evaluate: trace -> prop -> time -> three_valued}
[@@deriving yojson]

(* logical environment type
 *
 * The logical environment is an associate list, e.g., [(a,10); (b,20)]
 *)
type lenv =
  { mutable theta: (var_id * value) list
  ; eval: var_id -> value
  ; add: var_id -> value -> unit
  ; remove: var_id -> unit }
[@@deriving yojson]

let activate_debug = ref false

let count = ref 0

let count_duration = ref 0

(* Generating RMTLD formulas *)
(*
 * We use a set of parameters:
 * c_I, i_I, u_I, n_v, n_p, pr_u, pr_e, pr_<
 *
 * Generation Spec:
 *
 * - For Constant we use the normal distribution to select values (c_I is the interval of values)
 * - For Variable name we use letters of alphabet (n_v is the number of symbols)
 * - For Integral Value we use the same probability as terms and for formulas (pr_d)
 *
 * - For Prop name we use the alphabet letters (n_p is the number of propositions)
 * - For Until Value we use the normal distribution to select values (u_I is the interval of values, pr_u is the probability of chosing an until operator)
 * - For Exist name we use the alphabet letters (pr_e is the probability of chosing an exists)
 * - For < relation (pr_l is the probability of chosing a relation)
 * - For other formulas (not,or,true) the probability is the same
 *
 * - To chose a formula we use the same proability for other cases and pr_u, pr_e and pr_l for each formula until, exists and less
 * - To chose a term we use the same probability for other cases and pr_d for the case of duration term
 *
 * Default settings for intervals   : c_I:=[1,4], u_I:=[1,4]
 *                  for symbols     : n_v:=5, n_p:=5
 *                  for probability : pr_u:=0.3, pr_e:=0.2, pr_l:=0.4, pr_d:=0.2
 *                  for samples     : size:=500
 *)

(* gen_formula function *)
let gen_formula size (n_v, n_p, c_I, u_I, pr_u, pr_e, pr_l, pr_d) =
  let s_ l = if l >= 2 then 1 + Random.int (l - 2) else 1 in
  let t_ l s = l - s - 1 in
  let rec gen_term size =
    if size > 2 then
      let s = s_ size in
      match Random.float 1. with
      (*default p=.2, we have pr_d *)
      | x when x < pr_d -> Duration (gen_term s, gen_formula_ (t_ size s))
      | _ -> (
        match Random.int 2 with
        | 0 -> FPlus (gen_term s, gen_term (t_ size s))
        | 1 -> FTimes (gen_term s, gen_term (t_ size s))
        | _ -> raise (Failure "gen_term: bad random int") )
    else
      (* the formula could be small than n_samples *)
      match Random.int 2 with
      | 0 -> Constant (float_of_int (Random.int (snd c_I - 1) + 1))
      | 1 -> Variable ("v" ^ string_of_int (Random.int n_v))
      | _ -> raise (Failure "gen_term: bad random int <=2")
  and gen_formula_ size =
    let prop () = Prop ("p" ^ string_of_int (Random.int n_p)) in
    if size > 2 then
      (* Get sample for formula *)
      let s = s_ size in
      match Random.float 1. with
      | x when x < pr_u ->
          (* gen until formula *)
          Until
            ( float_of_int (Random.int (snd u_I - 1) + 1)
            , gen_formula_ s
            , gen_formula_ (t_ size s) )
      | x when x < pr_u +. pr_e ->
          (* gen exists formula *)
          Exists
            ("v" ^ string_of_int (Random.int n_v), gen_formula_ (size - 1))
      | x when x < pr_u +. pr_e +. pr_l ->
          (* gen less than formula *)
          LessThan (gen_term s, gen_term (t_ size s))
      | _ -> (
          let s = s_ size in
          match Random.int 2 with
          | 0 -> Not (gen_formula_ (size - 1))
          | 1 -> Or (gen_formula_ s, gen_formula_ (t_ size s))
          | _ -> raise (Failure "gen_formula_: bad random int") )
    else if size = 2 then Not (prop ())
    else prop ()
  in
  gen_formula_ size

(*
 * settings id: size (n_v,n_p, c_I,u_I, pr_u,pr_e,pr_l,pr_d);
 * note that: pr_u + pr_e + pr_l < 1 and pr_d < 1
 *)
let gen_formula_default () =
  gen_formula 20 (5, 5, (1, 4), (1, 4), 0.2, 0.1, 0.3, 0.3)

(* Functions for getting results about the search space *)

(* measuring formulas (n durations, n temporal operators) *)
let rec measure_term term =
  match term with
  | Constant _ -> (0, 0)
  | Variable _ -> (0, 0)
  | Duration (trm, sf) ->
      let x1, y1 = measure_term trm in
      let x2, y2 = measure_formula sf in
      (1 + x1 + x2, y1 + y2)
  | FPlus (eta1, eta2) ->
      let x1, y1 = measure_term eta1 in
      let x2, y2 = measure_term eta2 in
      (x1 + x2, y1 + y2)
  | FTimes (eta1, eta2) ->
      let x1, y1 = measure_term eta1 in
      let x2, y2 = measure_term eta2 in
      (x1 + x2, y1 + y2)

and measure_formula formula =
  match formula with
  | Prop _ -> (0, 0)
  | Not sf -> measure_formula sf
  | Or (sf1, sf2) ->
      let x1, y1 = measure_formula sf1 in
      let x2, y2 = measure_formula sf2 in
      (x1 + x2, y1 + y2)
  | Until (_, sf1, sf2) ->
      let x1, y1 = measure_formula sf1 in
      let x2, y2 = measure_formula sf2 in
      (x1 + x2, 1 + y1 + y2)
  | Exists (_, sf) -> measure_formula sf
  | LessThan (tr1, tr2) ->
      let x1, y1 = measure_term tr1 in
      let x2, y2 = measure_term tr2 in
      (x1 + x2, y1 + y2)
  | a ->
      raise
        (Failure ("Unsupported formula " ^ Sexp.to_string_hum (sexp_of_fm a)))

(* create an uniform trace of the type
 * (p1, (i1, i1')), ... ,(pn, (in, in')) list 
 *)
let rec generate_uniform_traces value samples lst =
  if List.length lst > samples then lst
  else
    let timestamp = Random.float 0.01 +. value in
    match Random.int 2 with
    | 0 ->
        generate_uniform_traces timestamp samples
          (("A", value (*, timestamp*)) :: lst)
    | 1 ->
        generate_uniform_traces timestamp samples
          (("B", value (*, timestamp*)) :: lst)
    | _ ->
        generate_uniform_traces timestamp samples
          (("C", value (*, timestamp*)) :: lst)

(* asymptotic function for complexity *)
let rec asym_comp (a, b, c) fm =
  match fm with
  | Until (_, fm1, fm2) when a <> [] ->
      fst
        (fold_left
           (fun (cts, lst) _ ->
             ( cts + asym_comp (lst, b, c) fm1 + asym_comp (lst, b, c) fm2
             , List.tl lst ) )
           (0, a) a )
  | Prop _ -> 1
  | _ -> raise (Failure "Not supported formula.")

(* generate an until formula using triangle pattern *)
let rec gen_u_formula_with_triangle_pattern rl size p =
  if size > 0 then
    Until
      ( p
      , gen_u_formula_with_triangle_pattern true (size - 1) p
      , gen_u_formula_with_triangle_pattern false (size - 1) p )
  else if rl then Until (p, Prop "A", Prop "B")
  else Until (p, Prop "A", Prop "*")

(* generate an until formula with maximum likelihood *)
let gen_u_formula_with_maximum_prop_evaluation size pval samples =
  let trc = generate_uniform_traces 0.2 samples [] in
  let rec gen_u_formula_with_maximum_prop_evaluation' size_op pval trc =
    (*print_endline ("D.. "^(string_of_int size_op));*)
    if not (size_op > 0) then Prop "A"
    else
      (* test if until of trees is better than the line of trees *)
      let fm1 =
        gen_u_formula_with_maximum_prop_evaluation' (size_op - 1) pval trc
      in
      let fm2 =
        gen_u_formula_with_maximum_prop_evaluation' (size_op - 2) pval trc
      in
      let _, n_to1 = measure_formula fm1 in
      let _, n_to2 = measure_formula fm2 in
      let fm3 =
        gen_u_formula_with_maximum_prop_evaluation'
          (size_op - n_to2 - 2)
          pval trc
      in
      let _, n_to3 = measure_formula fm3 in
      let fm1_tree =
        Until
          ( pval
          , fm1
          , gen_u_formula_with_maximum_prop_evaluation'
              (size_op - n_to1 - 1)
              pval trc )
      in
      let fm2_line =
        Until
          ( pval
          , Until
              ( pval
              , fm2
              , gen_u_formula_with_maximum_prop_evaluation'
                  (size_op - n_to2 - 2)
                  pval trc )
          , gen_u_formula_with_maximum_prop_evaluation'
              (size_op - n_to2 - n_to3 - 2)
              pval trc )
      in
      if
        asym_comp (trc, 0., 0.) fm1_tree < asym_comp (trc, 0., 0.) fm2_line
        && size_op - n_to2 - n_to3 - 2 >= 0
      then fm2_line
      else fm1_tree
  in
  gen_u_formula_with_maximum_prop_evaluation' size pval trc

(* 
 *  Pretty printers :
 *   - for traces
 *   - for formulas and terms
 *     * plaintext
 *     * latex
 *)

(* print trace *)
let rec print_trace trace =
  if length trace < 1 then ()
  else
    let x, y = hd trace in
    Printf.printf "(%s,%f), " x y ;
    print_trace (tl trace)

(* convert rmtld formulas to latex language *)
let rec slatex_of_rmtld_tm term =
  match term with
  | Constant value -> string_of_int (int_of_float value) ^ " "
  | Variable id -> id ^ " "
  | Duration (trm, sf) ->
      "\\int^{" ^ slatex_of_rmtld_tm trm ^ "} \\left("
      ^ slatex_of_rmtld_fm sf ^ "\\right) "
  | FPlus (eta1, eta2) ->
      "\\left( " ^ slatex_of_rmtld_tm eta1 ^ " + " ^ slatex_of_rmtld_tm eta2
      ^ "\\right)"
  | FTimes (eta1, eta2) ->
      "\\left( " ^ slatex_of_rmtld_tm eta1 ^ " * " ^ slatex_of_rmtld_tm eta2
      ^ "\\right)"

and slatex_of_rmtld_fm formula =
  match formula with
  | Prop p -> p ^ " "
  | Not sf -> "\\neg \\left(" ^ slatex_of_rmtld_fm sf ^ "\\right) "
  | Or (sf1, sf2) ->
      "\\left(" ^ slatex_of_rmtld_fm sf1 ^ "\\lor " ^ slatex_of_rmtld_fm sf2
      ^ "\\right)"
  | Until (pval, sf1, sf2) ->
      "\\left(" ^ slatex_of_rmtld_fm sf1 ^ "\\ U_{"
      ^ string_of_int (int_of_float pval)
      ^ "} \\ " ^ slatex_of_rmtld_fm sf2 ^ "\\right)"
  | Exists (var, sf) ->
      "\\exists {" ^ var ^ "} \\ \\left(" ^ slatex_of_rmtld_fm sf
      ^ "\\right)"
  | LessThan (tr1, tr2) ->
      "\\left(" ^ slatex_of_rmtld_tm tr1 ^ "< " ^ slatex_of_rmtld_tm tr2
      ^ "\\right)"
  | a ->
      raise
        (Failure ("Unsupported formula " ^ Sexp.to_string_hum (sexp_of_fm a)))

(* Print formulas and terms for latex *)
let print_latex_formula f = print_string (slatex_of_rmtld_fm f)

(* convert rmtld formulas to plain text *)
let rec string_of_rmtld_tm rmtld_tm =
  match rmtld_tm with
  | Constant value -> string_of_float value
  | Variable id -> id
  | Duration (trm, sf) ->
      "int[" ^ string_of_rmtld_tm trm ^ "] (" ^ string_of_rmtld_fm sf ^ ")"
  | FPlus (eta1, eta2) ->
      "( " ^ string_of_rmtld_tm eta1 ^ " + " ^ string_of_rmtld_tm eta2 ^ ")"
  | FTimes (eta1, eta2) ->
      "( " ^ string_of_rmtld_tm eta1 ^ " * " ^ string_of_rmtld_tm eta2 ^ ")"

and string_of_rmtld_fm rmtld_fm =
  match rmtld_fm with
  | True () -> "true"
  | Prop p -> p
  | Not sf -> "~(" ^ string_of_rmtld_fm sf ^ ")"
  | Or (sf1, sf2) ->
      "(" ^ string_of_rmtld_fm sf1 ^ " or " ^ string_of_rmtld_fm sf2 ^ ")"
  | Until (pval, sf1, sf2) ->
      "(" ^ string_of_rmtld_fm sf1 ^ " U["
      ^ (if pval = max_float then "infty" else string_of_float pval)
      ^ "] " ^ string_of_rmtld_fm sf2 ^ ")"
  | Since (pval, sf1, sf2) ->
      "(" ^ string_of_rmtld_fm sf1 ^ " S[" ^ string_of_float pval ^ "] "
      ^ string_of_rmtld_fm sf2 ^ ")"
  | Until_eq (pval, sf1, sf2) ->
      "(" ^ string_of_rmtld_fm sf1 ^ " U[=" ^ string_of_float pval ^ "] "
      ^ string_of_rmtld_fm sf2 ^ ")"
  | Since_eq (pval, sf1, sf2) ->
      "(" ^ string_of_rmtld_fm sf1 ^ " S[=" ^ string_of_float pval ^ "] "
      ^ string_of_rmtld_fm sf2 ^ ")"
  | Exists (var, sf) -> "exists " ^ var ^ " (" ^ string_of_rmtld_fm sf ^ ")"
  | LessThan (tr1, tr2) ->
      "(" ^ string_of_rmtld_tm tr1 ^ " < " ^ string_of_rmtld_tm tr2 ^ ")"
(*| a -> raise (Failure ("string_of_rmtld_fm: Unsupported formula " ^
  Sexp.to_string_hum (sexp_of_fm a)))*)

(* print formulas and terms in plaintext *)
let print_plaintext_formula f = print_string (string_of_rmtld_fm f)

(* Convert a trace into an observation set *)
let observation (trc : trace) (p : prop) (t : time) =
  try
    let v1 = List.find (fun (a, t1) -> t <= t1) trc in
    if p = fst v1 then True else False
  with Not_found -> Unknown

(* Environment record instantiation *)
let environment (trc : trace) : env = {trc; evaluate= observation}

(* Logical environment instantiation *)
let rec lenv =
  { theta= []
  ; eval= (fun var -> assoc var lenv.theta)
  ; add=
      (fun var value ->
        lenv.theta <- (var, value) :: lenv.theta ;
        () )
  ; remove=
      (fun var ->
        lenv.theta <- remove_assoc var lenv.theta ;
        () ) }

(* sub-trace function *)
let sub_k (k, _, t) gamma =
  (* check k size *)
  if length k.trc < 1 then []
  else
    let p1, _ =
      partition
        (fun (_, i1) -> if t <= i1 && i1 <= t +. gamma then true else false)
        k.trc
    in
    p1


(* auxiliar functions for doing conversions between this module and rmtld3_eval *)

let of_rmtld3_eval_tree_valued (a : Rmtld3_eval.three_valued) : three_valued
    =
  match a with
  | Rmtld3_eval.True -> True
  | Rmtld3_eval.False -> False
  | Rmtld3_eval.Unknown -> Unknown

let of_tree_valued (a : three_valued) : Rmtld3_eval.three_valued =
  match a with
  | True -> Rmtld3_eval.True
  | False -> Rmtld3_eval.False
  | Unknown -> Rmtld3_eval.Unknown

let of_rmtld3_eval_trace
    (operator :
         Rmtld3_eval.trace
      -> Rmtld3_eval.prop
      -> Rmtld3_eval.time
      -> Rmtld3_eval.three_valued ) : trace -> prop -> time -> three_valued =
 fun a b c -> of_rmtld3_eval_tree_valued (operator a b c)

let of_trace (operator : trace -> prop -> time -> three_valued) :
    trace -> prop -> time -> Rmtld3_eval.three_valued =
 fun a b c -> of_tree_valued (operator a b c)

let of_rmtld3_eval_env ({trc= i; evaluate= operator} : Rmtld3_eval.env) : env
    =
  {trc= i; evaluate= of_rmtld3_eval_trace operator}

let of_env ({trc= i; evaluate= operator} : env) : Rmtld3_eval.env =
  {trc= i; evaluate= of_trace operator}

let of_rmtld3_eval_duration (a : Rmtld3_eval.duration) : duration =
  match a with 
  | Rmtld3_eval.Dnone ->  Dnone
  | Rmtld3_eval.Dsome n -> Dsome n

let of_duration (a: duration) : Rmtld3_eval.duration =
  match a with 
  | Dnone ->  Rmtld3_eval.Dnone
  | Dsome n -> Rmtld3_eval.Dsome n

(* 
 * Interpretation of RMTLD3 terms and formulas
 * 
 * The `eval` function has the following inputs: the environment,
 *   the logical environment, the initial time, and the RMTLD formula.
 *
 * 'eval (env, lg_env, t) formula' has the following meaning:
 *   (env, lg_env, t) ⊨³ formula
 * 
 * The evaluation domain is three-valued (True, False, Unknown)
 *
 *)

let rec eval_term m t term =
  match term with
  | Constant value -> Dsome value
  | Duration (di, phi) -> eval_term_duration m t di phi
  | FPlus (tr1, tr2) -> (
    match (eval_term m t tr1, eval_term m t tr2) with
    | Dsome v1, Dsome v2 -> Dsome (v1 +. v2)
    | _ -> Dnone )
  | FTimes (tr1, tr2) -> (
    match (eval_term m t tr1, eval_term m t tr2) with
    | Dsome v1, Dsome v2 -> Dsome (v1 *. v2)
    | _ -> Dnone )
  | _ -> raise (Failure "eval_term: the term definition is missing")

and eval_term_duration (k, u) t tm fm =
  of_rmtld3_eval_duration
    (Rmtld3_eval.eval_tm_duration
       (fun k u t -> of_duration (eval_term (of_rmtld3_eval_env k, u) t tm))
       (fun k u t -> of_tree_valued (eval (of_rmtld3_eval_env k, u, t) fm))
       (of_env k) u t )

and eval (env, lg_env, t) formula =
  match formula with
  | True () -> True
  | Prop p ->
      (* counting proposition evaluation instead of recursive calls *)
      count := !count + 1 ;
      env.evaluate env.trc p t
  | Not sf -> b3_not (eval (env, lg_env, t) sf)
  | Or (sf1, sf2) ->
      b3_or (eval (env, lg_env, t) sf1) (eval (env, lg_env, t) sf2)
  | LessThan (tr1, tr2) ->
      b3_lessthan
        (eval_term (env, lg_env) t tr1)
        (eval_term (env, lg_env) t tr2)
  | Until (gamma, sf1, sf2) -> eval_uless (env, lg_env, t) gamma sf1 sf2
  | Until_eq (gamma, sf1, sf2) -> eval_ueq (env, lg_env, t) gamma sf1 sf2
  | Since (gamma, sf1, sf2) -> failwith "eval of since is not implemented!"
  | Since_eq (gamma, sf1, sf2) -> failwith "eval of since_eq is not implemented!"
  | Exists (_, sf) -> failwith "eval of exists is not implemented!"


and eval_uless m gamma phi1 phi2 =
  let k, u, t = m in
  of_rmtld3_eval_tree_valued
    (Rmtld3_eval.eval_uless gamma
       (fun k u t -> of_tree_valued (eval (of_rmtld3_eval_env k, u, t) phi1))
       (fun k u t -> of_tree_valued (eval (of_rmtld3_eval_env k, u, t) phi2))
       (of_env k) u t )

and eval_ueq m gamma phi1 phi2 =
let k, u, t = m in
of_rmtld3_eval_tree_valued
  (Rmtld3_eval.eval_ueq gamma
      (fun k u t -> of_tree_valued (eval (of_rmtld3_eval_env k, u, t) phi1))
      (fun k u t -> of_tree_valued (eval (of_rmtld3_eval_env k, u, t) phi2))
      (of_env k) u t )

(* 
 * compute the temporal upper bound of a RMTLD3 term and formula (if exists)
 *)
let rec calculate_t_upper_bound (formula : rmtld3_fm) =
  match formula with
  | True () -> 0.
  | Prop _ -> 0.
  | Not sf -> calculate_t_upper_bound sf
  | Or (sf1, sf2) ->
      Stdlib.max (calculate_t_upper_bound sf1) (calculate_t_upper_bound sf2)
  | Until (gamma, sf1, sf2) when gamma <> max_float ->
      gamma
      +. Stdlib.max
           (calculate_t_upper_bound sf1)
           (calculate_t_upper_bound sf2)
  | Until_eq (gamma, sf1, sf2) when gamma <> max_float ->
      gamma
      +. Stdlib.max
           (calculate_t_upper_bound sf1)
           (calculate_t_upper_bound sf2)
  | LessThan (tr1, tr2) ->
      Stdlib.max
        (calculate_t_upper_bound_term tr1)
        (calculate_t_upper_bound_term tr2)
  | Since (gamma, sf1, sf2) when gamma <> max_float ->
      gamma
      +. Stdlib.max
           (calculate_t_upper_bound sf1)
           (calculate_t_upper_bound sf2)
  | Since_eq (gamma, sf1, sf2) when gamma <> max_float ->
      gamma
      +. Stdlib.max
           (calculate_t_upper_bound sf1)
           (calculate_t_upper_bound sf2)
  | _ ->
      raise
        (Failure
           ( "ERROR: Attempt to acquire bound for unbound or unsupported \
              formula ("
           ^ Sexp.to_string (sexp_of_rmtld3_fm formula)
           ^ ")." ) )

and calculate_t_upper_bound_term term =
  match term with
  | Constant _ -> 0.
  | Duration (Constant c, fm) -> c +. calculate_t_upper_bound fm
  | Duration (tr, fm) ->
      calculate_t_upper_bound_term tr +. calculate_t_upper_bound fm
  | FPlus (tr1, tr2) ->
      Stdlib.max
        (calculate_t_upper_bound_term tr1)
        (calculate_t_upper_bound_term tr2)
  | FTimes (tr1, tr2) ->
      Stdlib.max
        (calculate_t_upper_bound_term tr1)
        (calculate_t_upper_bound_term tr2)
  | _ -> raise (Failure "Attempt to acquire bound for unsupported terms.")

(*
 * example function to generate results and gnuplot files to plot them
 *)
let example () =
  Random.self_init () ;
  (*Printf.printf "Random Seed Initialized !\n";*)
  activate_debug := false ;
  let activate_graph_generation = ref false in
  if activate_graph_generation = ref true then (
    (*print_latex_formula formula;*)

    (* time bins *)
    (*let bins = [(0.,0.01); (0.01,0.1); (0.1,0.2); (0.2,0.6); (0.6,0.8);
      (0.8,1.); (1.0,1.5); (1.5,2.); (2.,3.); (3.,5.)] in*)

    (* count bins *)
    let bins =
      [ (0., 10.)
      ; (10., 100.)
      ; (100., 1000.)
      ; (1000., 10000.)
      ; (10000., 100000.)
      ; (100000., 1000000.)
      ; (1000000., 10000000.) ]
    in
    let bins_array = Array.of_list bins in
    let bin_first, _ = hd bins in
    let _, bin_last = hd (rev bins) in
    let n_bins = length bins in
    let metrics_array = Array.make n_bins [] in
    let metrics (delta_t, count, n_to, n_du) =
      let v = count in
      (* create bins *)
      if bin_first >= v then
        metrics_array.(0) <- (delta_t, n_to, n_du) :: metrics_array.(0)
      else (
        if v > bin_last then
          metrics_array.(n_bins - 1) <-
            (delta_t, n_to, n_du) :: metrics_array.(n_bins - 1)
        else
          (* calculate bin to insert *)
          for i = 0 to n_bins - 1 do
            let b, b' = bins_array.(i) in
            if b <= v && v < b' then
              metrics_array.(i) <- (delta_t, n_to, n_du) :: metrics_array.(i)
          done ;
        () )
    in
    let fprint_metrics_list oc lst =
      fold_left
        (fun _ (x, y, z) -> Printf.fprintf oc "%f %i %i\\\\ " x y z)
        () lst ;
      ()
    in
    let fprint_metrics oc =
      (* print bin by line *)
      for i = 0 to n_bins - 1 do
        Printf.fprintf oc
          "\\addplot[boxplot] table[row sep=\\\\,y index=0] {" ;
        fprint_metrics_list oc metrics_array.(i) ;
        Printf.fprintf oc "};\n"
      done ;
      ()
    in
    let rec strategic_uniform_trace value samples factor trace =
      let trace_size = length trace in
      (*let timestamp = (Random.float factor) +. value in*)
      let timestamp = factor +. value in
      if samples = 0 then ("B", value (*, timestamp*)) :: trace
      else if samples <= trace_size then
        strategic_uniform_trace timestamp (samples - 1) factor
          (("B", value (*, timestamp*)) :: trace)
      else
        strategic_uniform_trace timestamp (samples - 1) factor
          (("A", value (*, timestamp*)) :: trace)
    in
    (* until case ... *)
    (* until(_,until(...),until(...)) *)
    let linear_plot_file = open_out "linear_plot.tex" in
    let ha = Array.of_list [1; 2; 3; 5; 10; 15] in
    (*let ha = Array.of_list [1; 2; 3; 4; 5; 6; 7; 8; 9; 10;] in*)
    (*let matrix_data = Array.of_list [(10, ha); (100, ha); (1000, ha);
      (10000, ha); (100000, ha);] in*)
    (*let matrix_data = Array.of_list [(10, ha); (100, ha); (1000, ha);] in*)
    let matrix_data = Array.of_list [(10, ha); (100, ha); (1000, ha)] in
    for st = 0 to Array.length matrix_data - 1 do
      let sizeof_trace, mheight = matrix_data.(st) in
      Printf.fprintf linear_plot_file
        "\\addplot[color=black,mark=o,solid,] table[row sep=\\\\,y index=2, \
         x index = 1]  {" ;
      for j = 0 to Array.length mheight - 1 do
        let height = mheight.(j) in
        (* construct trace and formula *)
        (*let trace = rev (generate_uniform_traces 0. sizeof_trace []) in*)
        (* strategic trace construction *)
        (* factor * sizeof_trace * 2 = duration -> factor = duration /
           (sizeof_trace*2) *)
        let duration = 10. in
        let trace =
          rev
            (strategic_uniform_trace 0. sizeof_trace
               (duration /. (float_of_int sizeof_trace /. 1.95))
               [] )
        in
        let formula =
          gen_u_formula_with_triangle_pattern true (height - 2) duration
        in
        (*((2. ** (float_of_int (height)))/. (float_of_int sizeof_trace)) *)
        (*print_trace trace ;*)
        Printf.printf "\nFormula with height=%i and |trace|=%i: \n  " height
          sizeof_trace ;
        (*print_plaintext_formula formula;*)
        Printf.printf "\n %!" ;
        let env = environment trace in
        (* generate environment based on trace *)
        let lg_env = lenv in
        (* logic environment -- not used when gen_quantifiers is false *)

        (* reset count *)
        count := 0 ;
        let time_start = Sys.time () in
        let _ev = eval (env, lg_env, 0.) formula in
        let time_end = Sys.time () in
        let delta_t = time_end -. time_start in
        Printf.fprintf linear_plot_file "%i %f %i %i %f\\\\ %!" !count
          delta_t height sizeof_trace
          (2. ** float_of_int height) ;
        ()
      done ;
      Printf.fprintf linear_plot_file "};\n"
    done ;
    close_out linear_plot_file ;
    let oc = open_out "data.tex" in
    let nsamples = 1000 in
    let sizeof_trace = 100 in
    (*gen_quantifiers := false;*)
    (* formulas without quantifiers; set probability p_e to 0. *)
    for i = 1 to nsamples do
      let formula =
        (* this generation manner is deprecated *)
        (*gen_formula height ((2. ** (float_of_int (height)))/. (float_of_int
          sizeof_trace)) in*)
        (* use "gen_formula height in" instead *)
        gen_formula_default ()
      in
      (* generate uniform traces *)
      let trace = rev (generate_uniform_traces 0. sizeof_trace []) in
      (*print_trace trace ;*)
      print_plaintext_formula formula ;
      let env = environment trace in
      (* generate environment based on trace *)
      let lg_env = lenv in
      (* logic environment -- not used when gen_quantifiers is false *)

      (* reset count *)
      count := 0 ;
      count_duration := 0 ;
      let time_start = Sys.time () in
      let _ev = eval (env, lg_env, 0.) formula in
      let time_end = Sys.time () in
      let n_du, n_to = measure_formula formula in
      let delta_t = time_end -. time_start in
      Printf.printf "%d)Duration %fs\nMetrics:\n" i delta_t ;
      Printf.printf "  Cardinality: %d\n" (length trace) ;
      Printf.printf "  Measure(temporal operators): %d\n" n_to ;
      Printf.printf "  Measure(duration terms): %d\n" n_du ;
      Printf.printf "  Count: %d\n" !count ;
      (*Printf.fprintf oc "%f; %d; %d\n" delta_t n_to n_du ;*)
      metrics (delta_t, float_of_int (!count + !count_duration), n_to, n_du) ;
      ()
    done ;
    fprint_metrics oc ;
    close_out oc ) ;
  ()
