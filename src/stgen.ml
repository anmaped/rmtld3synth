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
	Printf.printf "Elapsed: %f %f\n" (ex_time-.time) lifespan;
        if (ex_time-.time) > 0. then
        	sta_execution m new_state time ~trace:((symbol,lifespan)::trace) ex_time
	else rev trace

