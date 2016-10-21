
open Rmtld3
open Batteries

open Rmtld3synth_helper

let rmtld3synthsmt formula = 

	let common_types = "
(define-sort Proptype () Int)
(define-sort Time () Int)
(define-sort Duration () Time)
(declare-datatypes (T1 T2) ((Pair (mk-pair (first T1) (second T2)))))

(define-sort Trace () (Array Time Proptype) )

(declare-datatypes () ((Fourvalue FVTRUE FVFALSE FVUNKNOWN FVSYMBOL)))
(declare-datatypes () ((Threevalue TVTRUE TVFALSE TVUNKNOWN)))

(define-fun tvnot ((phi Threevalue)) Threevalue
	(ite (= phi TVTRUE) TVFALSE (ite (= phi TVFALSE) TVTRUE TVUNKNOWN) )
)

(define-fun tvor ((phi1 Threevalue) (phi2 Threevalue)) Threevalue
	(ite (or (= phi1 TVTRUE) (= phi2 TVTRUE) ) TVTRUE (ite (and (= phi1 TVFALSE) (= phi2 TVFALSE)) TVFALSE TVUNKNOWN ) )
)

(define-fun tvlessthan ((eta1 Duration) (eta2 Duration)) Threevalue
	(ite (< eta1 eta2) TVTRUE TVFALSE )
)
	" in

	(*  ( b31 == T_TRUE || b32 == T_TRUE ) ? T_TRUE : \\
      (( b31 == T_FALSE && b32 == T_FALSE ) ? T_FALSE : T_UNKNOWN) *)

      (* #define b3_lessthan(n1,n2) \\
    ( (std::get<1>(n1) || std::get<1>(n2))? T_UNKNOWN : ( ( std::get<0>(n1) < std::get<0>(n2) )? T_TRUE : T_FALSE ) )*)


	let map_macros = "
(define-fun mapb4 ( (phi Threevalue) ) Fourvalue

	(ite (= phi TVTRUE ) FVTRUE 
		(ite (= phi TVFALSE ) FVFALSE 
			FVUNKNOWN
	 	)
	 )
)

(define-fun mapb3 ( (x (Pair bool Fourvalue)) ) Threevalue

	(ite (and (= (first x) true) (= (second x) FVSYMBOL )) TVUNKNOWN 
		(ite (and (= (first x) false) (= (second x) FVSYMBOL )) TVFALSE 
			(ite (= (second x) FVFALSE ) TVFALSE 
				TVTRUE
	 		)
	 	)
	 )
)
	" in

	let evali = "
(define-fun evali ((b1 Threevalue) (b2 Threevalue)) Fourvalue
	(ite (not (= b2 TVFALSE)) (mapb4 b2) (ite (and (not (= b1 TVTRUE)) (= b2 TVFALSE)) (mapb4 b1) FVSYMBOL ) )
)
	" in


	let new_trace = "
(declare-const trc Trace )
(declare-const trc_size Time)
	" in


	let compute_proposition id = "
(define-fun computeprop" ^ id ^ " ( (mk Trace) (mt Time) (phi1 Proptype) ) Threevalue
	(ite (= (select mk mt) phi1) TVTRUE TVFALSE  );
)
	" in


	let compute_until id t gamma (comp1, comp1_append) (comp2, comp2_append) =
		let evalb id comp1 comp2 = "
(define-fun evalb" ^ id ^ "  ( (mk Trace) (mt Time) (v Fourvalue) ) Fourvalue
	(ite (= v FVSYMBOL) (evali "^ comp1 ^" "^ comp2 ^" ) v )
)
		" in

		let evalfold id = "
(declare-fun evalfold" ^ id ^ " ( (Time) (Time)) Fourvalue )
(assert (forall ((x Time) (i Time)) (=> (and (>= x 0) (<= x "^ (string_of_int (gamma + t)) ^")) (ite
	(and (and (< x "^ (string_of_int (gamma + t)) ^") (>= i 0)) (> x i))
	(= (evalb" ^ id ^ " trc x (evalfold" ^ id ^ " (- x 1) i )) (evalfold" ^ id ^ " x i )  )
	(= (evalb" ^ id ^ " trc x FVSYMBOL) (evalfold" ^ id ^ " x i))
   )))
)
		" in 
		let evalc id = "
(define-fun evalc" ^ id ^ " ((mt Time) (mtb Time)) (Pair Bool Fourvalue)
	(mk-pair (<= trc_size " ^ (string_of_int gamma) ^ ") (evalfold" ^ id ^ " (- mt 1) mtb ))
)
		" in comp1_append ^ comp2_append ^ (evalb id comp1 comp2) ^ (evalfold id) ^ (evalc id) ^ "
(define-fun computeUless" ^ id ^ "  ((mt Time) (mtb Time)) Threevalue
	(mapb3 (evalc" ^ id ^ " mt mtb))
)
	" in

	let duration t dt formula =
		let indicator = "
(define-fun indicator ((mk Trace) (mt Time)) Int
	(ite (= "^ formula ^" TVTRUE) 1 0)
)
		" in
		let evaleta =
"
(declare-fun evaleta ((Time) (Time)) Int)
(assert (forall ((x Time) (i Time)) (=> (and (>= x 0) (<= x (+ "^ (string_of_int t) ^" "^ dt ^") )) (ite
	(and (and (< x (+ "^ (string_of_int t) ^" "^ dt ^") ) (>= i 0)) (> x i))
	(= (evaleta x i) (+ (evaleta (- x 1) i) (indicator trc x) ))
	(= (evaleta x i) (indicator trc x) )
	)))
)
		" in
	("(computeduration "^ dt ^" "^ (string_of_int t) ^")", indicator^evaleta^"
(define-fun computeduration ((mt Time) (mtb Time)) Duration
	(evaleta (- mt 1) mtb)
)
	")
	in


(* unfold formula *)
let rec compute_term t term helper =
  match term with
    | Constant value       -> (string_of_int(int_of_float value), "")
    | Duration (di,phi)    -> let tr_out1, tr_out2 = compute_term t di helper in
    						  let sf_out1, sf_out2 = compute t phi helper in
    						  let dur_out1, dur_out2 = duration t tr_out1 sf_out1 in
    						  (dur_out1, tr_out2^sf_out2^dur_out2)
    | FPlus (tr1,tr2)      -> ("", "")
    | FTimes (tr1,tr2)     -> ("", "")
    | _ -> raise (Failure "compute_terms: missing term")
and compute t formula helper =
  match formula with
    | Prop p                  -> let tbl = get_proposition_hashtbl helper in
                                 let counter = get_proposition_counter helper in 
                                 let val1,val2 = try (Hashtbl.find tbl p,"") with Not_found -> Hashtbl.add tbl p counter; (counter, compute_proposition (string_of_int counter)) in
                                 ("(computeprop"^ (string_of_int val1) ^" mk mt "^ (string_of_int val1) ^")", val2)

    | Not sf                  -> let sf_out1, sf_out2 = compute t sf helper in
    							 ("(tvnot "^ sf_out1 ^" )", sf_out2)

    | Or (sf1, sf2)           -> let sf1_out1, sf1_out2 = compute t sf1 helper in
    							 let sf2_out1, sf2_out2 = compute t sf2 helper in
    							 ("(tvor "^ sf1_out1 ^" "^ sf2_out1 ^")", sf1_out2^sf2_out2)

    | Until (gamma, sf1, sf2) -> (*let range = (List.range itv_low `To itv_upp )  in*)
    							 let idx = get_until_counter helper in
    							 let sf1_out1, sf1_out2 = (compute (t + (int_of_float gamma)) sf1 helper) in
    							 let sf2_out1, sf2_out2 = (compute (t + (int_of_float gamma)) sf2 helper) in
    	 					     (
    								"(computeUless!" ^ (string_of_int idx) ^" "^ (string_of_int (int_of_float gamma)) ^" "^ (string_of_int t) ^")"
    							  ,
							    	(compute_until
							    		("!"^(string_of_int idx))
							    		t
							    		(int_of_float gamma)
							    		(sf1_out1, sf1_out2)
							    		(sf2_out1, sf2_out2)
							    	)
								 )

    | LessThan (tr1,tr2)      -> let tr1_out1, tr1_out2 = (compute_term t tr1 helper) in
    							 let tr2_out1, tr2_out2 = (compute_term t tr2 helper) in
    							 ("(tvlessthan "^ tr1_out1 ^" "^ tr2_out1 ^")", tr1_out2^tr2_out2)
    | _ -> raise (Failure "compute: missing formula") in


(* call the unfold function *)
(*let formula2 = Until(10., Prop("A"), Prop("B")) in
let formula = Until(10., Until(10., Prop("A"), Prop("B")), Prop("B")) in*)

let helper = ("", "", ref 0, ([],[],[]), [(ref 0, Hashtbl.create 10); (ref 0, Hashtbl.create 10)]) in
let toassert,x = compute 0 formula helper in

common_types ^ map_macros ^ evali ^ new_trace ^ x ^ "
(assert (= "^ toassert ^" TVTRUE) )
" ^"
(check-sat)
(get-model)
"
