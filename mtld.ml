(* STOCHASTIC TRACE GENERATOR MODULE
 *
 * The following implementation of the stochastic trace generator uses a stochastic automata
 * as input model. The model is defined by a set of funtions:
         * An initial state
         * A final state function
         * A list of Symbols
         * A symbol feasible function
         * A transition funcion
         * A stochastic clock set
 *
 * The StochasticTraceGenerator model is initialized by sta_execution
 * function. The function has a model, an initial state, an initial time
 * instant, and a threshold simulation time as inputs.
 *
 * *)

open List

(* Module to generate traces from a stochastic model *)
module StochasticTraceGenerator =
struct

exception Deadlock

(* Stochastic Automata definition STA *)
type ('ostate,'event,'pdist) sta = {
        initial   : 'ostate ;
        final     : 'ostate -> bool ;
        symbols   : 'event list;
        active    : 'ostate * 'event -> bool ;
        next      : 'event -> 'ostate -> 'ostate ;
        st_clocks : 'event -> 'pdist ;
}

(* Trace definition *)
type ('event) tuple = 'event * float ;;

(* assigning the symbols probabilistically - weibull and exponential distributions *)
let sampling model symbol = 
        match (model.st_clocks symbol) with
        | `weibull (lambda,k) -> lambda *. (((-1.) *. (log (1. -. (Random.float 1.)))) ** (1. /. k))
        | `exp (gamma) ->  (-1.) *. gamma *. log (1. -. (Random.float 1.))

let feasible_symbols model list_of_symbols state =
        fold_left (fun x a -> if (model.active (state,a)) then (a, (sampling
        model a))::x else x) [] list_of_symbols

let trigger_symbol = function
	  [] -> raise Deadlock
	| x::xs -> fold_left (fun (xs,xt) (ass,at) -> if (xt < at) then (ass,at) else (xs,xt))
        x xs

(* STA Execution *)
let rec sta_execution m state t ?(trace=[]) ex_time =
        let feasible_symbols_list = feasible_symbols m m.symbols state in
        (* If feasible_symbols_list is empty then there is a deadlock. *)
        let (symbol,lifespan) = trigger_symbol feasible_symbols_list  in
        let time = t +. lifespan in
        let new_state = m.next symbol state in
	let () = Printf.printf "Elapsed: %f %f\n" (ex_time-.time) lifespan in 
        if (ex_time-.time) > 0. then
        	sta_execution m new_state time ~trace:((symbol,lifespan)::trace) ex_time
	else rev trace

end;;

(* METRIC TEMPORAL LOGIC WITH DURATIONS CHECK MODULE
 *
 * The MTLD syntax and semantics were implemented in this module. The evaluation
 * of a logical formula is given by the models function. This function has an
 * observation function, a logical environment, an initial time, and a MTLD
 * formula as inputs. This model function has the following mean:
         * (observation, logical environment, t) models the MTLD formula.
 *
 * The meaning of the models operator is ensured if the observation, the logical
 * environment, and the time instant checks the MTLD formula. 
 *
 * *)

(* Mtld Module *)
module Mtld =
struct

exception UndefinedFormula
exception UndefinedTerm

type var_id = string
type prop   = string
type time   = float
type value  = float
type fsig   = [ `equal | `less ]

type phi =
          Prop of prop
        | Not of phi
        | Or of phi * phi
        | Until of fsig * time * phi * phi
        | Exists of var_id * phi
        | Leq of term * term
and term =
          Const of value
        | Var of var_id
        | Integral of term * phi
        | FTimes of term * term
        | FPlus of term * term
        | FMinus of term * term
        | FDiv of term * term

(* Observation/environment record type *)
type obs = {
        cumulated_trace         : (prop * time) list ;
        duration_of_trace       : time ;
        evaluate                : prop -> time -> bool ;
        interval                : time -> bool ;
	trace_cardinality       : int ;
}

(* Logic observation/environment record type *)
type logic_obs = {
	mutable theta : (var_id * value) list ;
	eval          : var_id -> value ;
	add           : var_id -> value -> unit ;
	remove        : var_id -> unit ;
}

(* Convert a trace into an observation set *)
let observation trace p t =
	exists2 (fun (e1,t1) (e2,t2) -> (t1<=t && t2>t) && (p=e1)) trace (rev
        (("infty",infinity)::(rev (tl trace))))

(* Convert the trace into an equivalent cumulated trace *)
let cumulate trace =
        let symbol,symbol_timespan = hd trace in
        rev (let x,_,_ = fold_left (fun (a,sum,yold) (x,y) -> let sumx = yold +. sum in
        ((x,sumx)::a,sumx,y)) ([(symbol, 0.)],0.,symbol_timespan) (tl trace) in x)

(* Get the time stamps of the observation trace; the events only change in that instants. *)
let cumulated_instants cumulated_trace =
        rev (fold_left (fun sum (_,a) -> a::sum) [] cumulated_trace)

(* Environment record instantiation *)
let rec environment trace = {
        cumulated_trace = cumulate trace;
        duration_of_trace = (function [] -> 0. | x::xs -> fold_left (fun a (_,x)
        -> a +. x) 0. (x::xs)) trace;
	evaluate = observation (cumulate trace);
	interval = (fun t -> t <= ((environment trace).duration_of_trace));
	trace_cardinality = length trace;
}

(* Logical environment instantiation *)
let rec logical_environment = {
	theta = []; (* Example of a environment: [(a,10); (b,20)]*)
	eval = (fun var -> assoc var logical_environment.theta );
	add = (fun var value ->  logical_environment.theta <- (var,value)::logical_environment.theta; () );
	remove = (fun var -> logical_environment.theta <- remove_assoc var logical_environment.theta; () );
}




(* Implementation of the Riemann Integrability *)
(* Riemann Sums *)
let riemann_sums f p =
	fold_left ( fun sum (xold,x) -> 
		      let tmp = x -. xold in
		      let t = x -. (tmp *. Random.float 1.0) in
		      sum +. ((f t) *. tmp)
	) 0. p

(* Create a partition by selecting the time instants when event changes *)
let riemann_partition partitions (c,d) = 
        if d > c then
                rev ((function
		  [] -> invalid_arg "Error: List of event time instants is empty."
		| x::xs ->  fold_left2 (fun a t1 t2 -> if c <= t1 && t2 <= d then (t1,t2)::a else a) [] (rev (tl (rev (x::xs)))) xs
		) partitions) else invalid_arg "Error: c >= d."

let supremum = function
	  [] ->  invalid_arg "empty list"
	| x::xs -> List.fold_left max x xs

let infimum = function
	  [] ->  invalid_arg "empty list"
	| x::xs -> List.fold_left min x xs

(* The Upper Riemann Sums *)
let upper_riemann_sum partition f =
	fold_left (fun x a -> let (x1,x2) = supremum (riemann_partition
        partition a) in x +. ((f x1) *. (x2 -. x1)) ) 0. (riemann_partition
        partition (0., infinity))

(* The Lower Riemann Sums *)
let lower_riemann_sum partition f =
        fold_left (fun x a -> let (x1,x2) = infimum (riemann_partition partition
        a) in x +. ((f x1) *. (x2 -. x1)) ) 0. (riemann_partition partition (0., infinity))

(* Supremum of Lower Riemann Sums *)
let supremum_of_lower_riemann_sums env f =
        lower_riemann_sum (cumulated_instants env.cumulated_trace) f
        (* Generate other partitions -> change (cumulated_instants
         * env.cumulated_trace) to other list of partitions *)

(* Infimum of Upper Riemann Sums *)
let infimum_of_upper_riemann_sums env f =
        upper_riemann_sum (cumulated_instants env.cumulated_trace) f
        (* Generate other partitions -> change (cumulated_instants
         * env.cumulated_trace) to other list of partitions *)

(* Is Riemann Integrable *)
let is_riemann_integrabble env f = (supremum_of_lower_riemann_sums env f) =
        (infimum_of_upper_riemann_sums env f)

(* Boolean observation into real function *)
let real_observation_function env p t = ((fun a -> if a then 1. else 0.) (env.evaluate p t))


(* Generate bounded values *)
let rec gen_float_bounded_values bound granularity = function
	  [] -> invalid_arg "Need initialization value."
	| x::xs -> if x < bound then gen_float_bounded_values bound granularity ((x +. granularity)::(x::xs)) else x::xs 


let rec models_terms (env, lg_env) t term =
	match term with
	| Const value -> value
	| Var id      -> lg_env.eval id
	| Integral (inext,sf)  -> riemann_sums (fun tt -> if (models (env, lg_env, tt) sf) = true then (*let () = Printf.printf "One\n" in*) 1. else (* let () = Printf.printf "zero\n" in*)  0.) (* Use sf in fun t *)
        (riemann_partition (cumulated_instants env.cumulated_trace) (t, t +.
        (models_terms (env, lg_env) t inext)))
	| FPlus (tr1,tr2)  -> models_terms (env, lg_env) t tr1 +.  models_terms
        (env, lg_env) t tr2
	| FMinus (tr1,tr2) -> models_terms (env, lg_env) t tr1 -.  models_terms
        (env, lg_env) t tr2
        | FTimes (tr1,tr2) -> models_terms (env, lg_env) t tr1 *.  models_terms
        (env, lg_env) t tr2
	| FDiv (tr1,tr2) -> models_terms (env, lg_env) t tr1 /.  models_terms
        (env, lg_env) t tr2
	(*| _ -> raise UndefinedTerm*)


and models (env, lg_env, t) formula =
        match formula with
        | Prop p           -> env.evaluate p t && env.interval(t)
        | Not sf           -> not (models (env, lg_env, t) sf)
        | Or (sf1, sf2)    -> models (env, lg_env, t) sf1 || models (env, lg_env, t) sf2
        | Until (`less, pval, sf1, sf2) -> (
		try let t' = find (fun a -> models (env,lg_env,a) sf2) (gen_float_bounded_values ((t +. pval) -. epsilon_float) 0.1 [t]) in
		for_all (fun t'' -> models (env,lg_env,t'') sf1) (gen_float_bounded_values (t' -. epsilon_float) 0.1 [t])
		with Not_found -> false
	  )
	| Until (`equal, pval, sf1, sf2) -> true
(* Continue... *)	
	  (*try
	    let t' = find (fun a -> models (env,lg_env,a) sf2) (gen_float_bounded_values ((t +. pval) -. epsilon_float) 0.1 [t]) in
	    for_all (fun a -> true) (gen_float_bounded_values (t' -. epsilon_float) 0.1 [t])
	  with Not_found -> false*)
	(*| Until ("E",pval, sf1,sf2) -> models (env,lg_env,t+pval) sf2 &&*)
(* if there exists a t' such that (t <= t') and sn(t',(t+pval)) model (env, lg_env, t') sf2 && forall t'' t <= t'' and t'' < t' model (env,lg_env,t'') sf1 *)
	
        | Exists (var,sf)  -> exists (fun num -> let () = lg_env.add var num in models (env,lg_env,t) sf ) (gen_float_bounded_values 100. 0.1 [0.])
        | Leq (tr1,tr2)    -> (models_terms (env, lg_env) t tr1) <=
                (models_terms (env, lg_env) t tr2)
        (*| _ -> raise UndefinedFormula*)


end;;

open List
open StochasticTraceGenerator
open Mtld

exception UndefinedSymbol

let _ =
        (* The Model *)
	let model = {
		initial = `state1;
		final = (function `state2 -> true | _ -> false);
		active = (function (`state1,"task1") -> true | (`state2,"task2")
                -> true | (`state3,"task2") -> true | (_,_) -> false);
                symbols = ["task1"; "task2"];
		next = (function
			| "task1" -> (function `state1 -> `state2 | `state2 -> `state1 | _ -> `error)
			| "task2" -> (function `state1 -> `state1 | `state2 ->
                                        `state3 | `state3 -> `state1 | _ -> `error)
                        | _ -> raise UndefinedSymbol
		);
		st_clocks = (function
			| "task1" -> `weibull (1.0,0.5)
			| "task2" -> `exp (1.0)
                        | _ -> raise UndefinedSymbol
                );
	} in
        let () = Random.self_init () in
        let () = Printf.printf "Random Seed Initialized !\n" in
	let stop_time = 100. in
        let trace = sta_execution model model.initial 0. stop_time in
	let env = environment trace in
	
	(* Integrability check of observations *)
	let () = fold_left (fun a x -> if is_riemann_integrabble env (real_observation_function env x) then (Printf.printf ("The observation of the proposition -%s- is integrable.\n") x) else Printf.printf ("Integrability of %s was not checked!\n") x ) () model.symbols in

	(* Run Evalutation function *)
	let lg_env = logical_environment in
	(*models (env, lg_env, 0.) (Or (Prop "task1",Prop "task2"))*)

	(* Check example:
	   Exists a (Or (Prop "task1", Prop "task2"))
	 *)

	(* Examples to evaluate:
	 *
	 * Abreviations:
	 *  - false = And (Prop "", Not(Prop ""))
	 *  - true = Not (false)
	 *  - And x y =  Not (Or (Not (x), Not(y)))
	 *
	 * true U[< t] phi => Until(`less, t, true, Prop "phi")
	 * 
	 * *)
	
	(* abreviations *)
	let mand phi1 phi2 = Not (Or (Not phi1, Not phi2)) in
	let mfalse = mand (Prop "") (Not (Prop "")) in
	let mtrue = Not mfalse in
	let mimplies phi1 phi2 = Not (Or (phi1, Not phi2)) in
	let meventually t phi = Until (`less, t, mtrue, phi) in 
	let malways t phi = Not (meventually t (Not phi)) in
	
	let () = if models (env, lg_env, 0.) mtrue=true then Printf.printf "true\n" in (* Cool it works :D *)
	let time_start = Sys.time () in
	
	(* Formulas *)
	let t = stop_time in
	let beta = 10. in
	let formula1 = Until (`less, stop_time, mtrue, Prop "phi") in
	let formula2 = mimplies (Prop "phi") (malways t (Prop "phi2")) in
	let formula3 = mimplies (Prop "phi") (meventually t (Prop "phi2")) in
	let formula4 =  malways t (Leq (Integral (Const t, Prop "phi"), Const beta)) in
	let formula5 =  mimplies (Prop "phi") (Leq (Integral (Const t, Prop "psi"), Const beta)) in
	
	let ev = models (env, lg_env, 0.) formula4 in
	let time_end = Sys.time () in
	let () = Printf.printf "%f %fs\n" (time_end -. time_start) time_start in
	let () = Printf.printf "%d\n" (length trace) in
	if ev=true then Printf.printf "trace is accepted by model!\n" else Printf.printf "tace is rejected by model!\n"
		

;;



