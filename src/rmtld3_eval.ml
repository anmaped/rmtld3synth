(* 
 * Three-valued Restricted Metric Temporal Logic with Durations Evalutaion Module
 *)

open List

type var_id = string

type prop = string

type time = float

type value = float

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

(*
 *  trace is an associate list with n elements of the form
 *    (prop, duration_1), ..., (prop, duration_n)
 *)
type trace = (prop * time) list

(* observation type (propositional part) *)
type env = {trc: trace; evaluate: trace -> prop -> time -> three_valued}

type lenv =
  { mutable theta: (var_id * value) list
  ; eval: var_id -> value
  ; add: var_id -> value -> unit
  ; remove: var_id -> unit }

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

(* eval_tm_duration *)
let eval_tm_duration tm fm k u t =
  let dt = (t, tm k u t) in
  let indicator_function (k, u) t phi = if fm k u t = True then 1. else 0. in
  let riemann_sum m dt (i, i') phi =
    (* dt=(t,t') and t in ]i,i'] or t' in ]i,i'] *)
    let t, t' = dt in
    if i <= t && t < i' then
      (* lower bound *)
      (i' -. t) *. indicator_function m t phi
    else if i <= t' && t' < i' then
      (* upper bound *)
      (t' -. i) *. indicator_function m t' phi
    else (i' -. i) *. indicator_function m i phi
  in
  let eval_eta m dt phi x =
    let rec fold lst acc =
      if length lst <= 1 then acc
      else if tl lst = [] then acc
      else
        fold (tl lst)
          (riemann_sum m dt (snd (hd lst), snd (hd (tl lst))) phi +. acc)
    in
    fold x 0.
  in
  match dt with
  | t, Dsome t' when List.exists (fun (_, a) -> a >= t +. t') k.trc ->
      Dsome (eval_eta (k, u) (t, t') fm (sub_k (k, u, t) t'))
  | _ -> Dnone

(* eval_uless *)
let eval_uless gamma f1 f2 k u t =
  let m = (k, u, t) in
  let eval_i b1 b2 =
    if b2 <> False then b3_to_b4 b2
    else if b1 <> True then b3_to_b4 b1
    else Symbol
  in
  let eval_b (k, u, t) f1 f2 v =
    if v <> Symbol then v else eval_i (f1 k u t) (f2 k u t)
  in
  let eval_fold (k, u, t) f1 f2 x =
    let rec fold lst (v, t') =
      if length lst <= 1 then (v, t')
      else if tl lst = [] then (v, t')
      else fold (tl lst) (eval_b (k, u, t') f1 f2 v, snd (hd (tl lst)))
    in
    fst (fold x (Symbol, t))
  in
  if not (gamma >= 0.) then
    raise (Failure "Gamma of U< operator is a non-negative value")
  else
    let k, _, t = m in
    let subk = sub_k m gamma in
    let eval_c = eval_fold m f1 f2 subk in
    if eval_c = Symbol then
      if List.exists (fun (_, a) -> a >= t +. gamma) k.trc then False
      else Unknown
    else b4_to_b3 eval_c

(* differs from uless on "eval_b" and fold *)
let eval_ueq gamma f1 f2 k u t =
  let m = (k, u, t) in
  let eval_i b1 b2 =
    if b2 <> False then b3_to_b4 b2
    else if b1 <> True then b3_to_b4 b1
    else Symbol
  in
  let eval_b (k, u, t) f1 f2 v =
    if v <> Symbol && v <> STrue then v else eval_i (f1 k u t) (f2 k u t)
  in
  let eval_fold (k, u, t) f1 f2 x =
    let rec fold lst (v, t') =
      if length lst <= 0 then (v, t')
      else if tl lst = [] then (eval_b (k, u, t') f1 f2 v, t')
      else fold (tl lst) (eval_b (k, u, t') f1 f2 v, snd (hd (tl lst)))
    in
    fst (fold x (Symbol, t))
  in
  if not (gamma >= 0.) then
    raise (Failure "Gamma of U= operator is a non-negative value")
  else
    let k, _, t = m in
    let subk = sub_k m gamma in
    let eval_c = eval_fold m f1 f2 subk in
    if eval_c = Symbol then
      if List.exists (fun (_, a) -> a >= t +. gamma) k.trc then False
      else Unknown
    else b4_to_b3 eval_c

(* differs from uless on fold *)
let eval_ulesseq gamma f1 f2 k u t =
  let m = (k, u, t) in
  let eval_i b1 b2 =
    if b2 <> False then b3_to_b4 b2
    else if b1 <> True && b2 = False then b3_to_b4 b1
    else Symbol
  in
  let eval_b (k, u, t) f1 f2 v =
    if v <> Symbol then v else eval_i (f1 k u t) (f2 k u t)
  in
  let eval_fold (k, u, t) f1 f2 x =
    let rec fold lst (v, t') =
      if length lst <= 0 then (v, t')
      else if tl lst = [] then (eval_b (k, u, t') f1 f2 v, t')
      else fold (tl lst) (eval_b (k, u, t') f1 f2 v, snd (hd (tl lst)))
    in
    fst (fold x (Symbol, t))
  in
  if not (gamma >= 0.) then
    raise (Failure "Gamma of U<= operator is a non-negative value")
  else
    let k, _, t = m in
    let subk = sub_k m gamma in
    let eval_c = eval_fold m f1 f2 subk in
    if eval_c = Symbol then
      if List.exists (fun (_, a) -> a >= t +. gamma) k.trc then False
      else Unknown
    else b4_to_b3 eval_c
