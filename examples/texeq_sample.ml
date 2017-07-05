(*
   automatically generated code from rmtld3synth Git version v0.2-alpha-46-g38ff318 (38ff318181783ef464b346f42a37ef72d08340a8)
   cmd: ./rmtld3synth --synth-ocaml --input-latexeq "(a \rightarrow ((a \lor b) \until_{<10} c)) \land \int^{10} (c \lor d) < 4"
*)

open List
open Rmtld3

module type Trace = sig val trc : trace end
(* one trace :: module OneTrace : Trace = struct let trc = [("a",(1.,2.))] end *)

module Mon0  ( T : Trace  ) = struct


let compute_uless gamma f1 f2 k u t =
  let m = (k,u,t) in
  let eval_i b1 b2 =
    if b2 <> False then
      b3_to_b4 b2
    else if b1 <> True && b2 = False then
      b3_to_b4 b1
    else
      Symbol
  in

  let eval_b (k,u,t) f1 f2 v =
    if v <> Symbol then
      v
    else
      eval_i (f1 k u t) (f2 k u t)
  in

  let eval_fold (k,u,t) f1 f2 x =
    fst (fold_left (fun (v,t') (prop,(ii1,ii2)) -> (eval_b (k, u, t') f1 f2 v, ii2)) (Symbol,t) x)
  in

  if not (gamma >= 0.) then
    raise  (Failure "Gamma of U operator is a non-negative value")
  else
  begin
    let k,_,t = m in
    let subk = sub_k m gamma in
    let eval_c = eval_fold m f1 f2 subk in
    if eval_c = Symbol then
      if k.duration_of_trace <= (t +. gamma) then
        Unknown
      else (
        False
      )
    else
      b4_to_b3 eval_c
  end


let compute_tm_duration tm fm k u t =
  let dt = (t,tm k u t) in

  let indicator_function (k,u) t phi = if fm k u t = True then 1. else 0. in
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
  eval_eta (k,u) dt fm (sub_k (k,u,t) t')

  let env = environment T.trc
  let lg_env = logical_environment
  let t = 0.
  let mon = (fun k s t -> b3_not ((fun k s t -> b3_or ((fun k s t -> b3_not ((fun k s t -> b3_or ((fun k s t -> b3_not ((fun k s t -> k.evaluate k.trace "a" t) k s t)) k s t) ((compute_uless 10. (fun k s t -> b3_or ((fun k s t -> k.evaluate k.trace "a" t) k s t) ((fun k s t -> k.evaluate k.trace "b" t) k s t)) (fun k s t -> k.evaluate k.trace "c" t)) k s t)) k s t)) k s t) ((fun k s t -> b3_not ((fun k s t -> b3_lessthan ((compute_tm_duration (fun k s t -> 10.) (fun k s t -> b3_or ((fun k s t -> k.evaluate k.trace "c" t) k s t) ((fun k s t -> k.evaluate k.trace "d" t) k s t))) k s t) ((fun k s t -> 4.) k s t)) k s t)) k s t)) k s t)) env lg_env t
end
