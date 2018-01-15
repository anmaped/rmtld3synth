
open Rmtld3
open Batteries
open Sexplib
open Sexplib.Conv


open Rmtld3synth_helper

let rmtld3synthsmt formula helper = 

	let common_types = "
(set-option :auto_config false) ; Usually a good idea
(set-option :model.v2 true)
(set-option :smt.phase_selection 0)

(set-option :smt.restart_strategy 0)
(set-option :smt.restart_factor |1.5|)

(set-option :smt.arith.random_initial_value true)
(set-option :smt.case_split 10)

(set-option :smt.delay_units true)
(set-option :smt.delay_units_threshold 300)

(set-option :smt.qi.eager_threshold 400)


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
				(ite (= (second x) FVTRUE ) TVTRUE TVUNKNOWN)
	 		)
	 	)
	 )
)
	" in

	let evali = "
(define-fun evali ((b1 Threevalue) (b2 Threevalue)) Fourvalue
	(ite (= b2 TVFALSE) (ite (= b1 TVTRUE) FVSYMBOL (mapb4 b1) ) (mapb4 b2) )
)
	" in


	let new_trace = "
(declare-const trc Trace )
(declare-const trc_size Time)
	" in


	let compute_proposition id = "
(define-fun computeprop" ^ id ^ " ( (mk Trace) (mt Time) ) Threevalue
	(ite (= (select mk mt) "^ id ^") TVTRUE TVFALSE  );
)
	" in


(*
  synthesis of until less than
*)
	let compute_until_less id t gamma (comp1, comp1_append) (comp2, comp2_append) =
		let evalb id comp1 comp2 = "
(define-fun evalb" ^ id ^ "  ( (mk Trace) (mt Time) (v Fourvalue) ) Fourvalue
	(ite (= v FVSYMBOL) (evali "^ comp1 ^" "^ comp2 ^" ) v )
)
		" in

		let evalfold id = if gamma > 1 then "
(declare-fun evalfold" ^ id ^ " ( (Time) (Time)) Fourvalue )
(assert (forall ((x Time) (i Time))
  (ite (and (>= i 0) (and (>= x 0) (<= x "^ (string_of_int (gamma + t)) ^")) )
    (ite (> x i)
	  (= (evalb" ^ id ^ " trc x (evalfold" ^ id ^ " (- x 1) i )) (evalfold" ^ id ^ " x i ) )
	  (= (evalb" ^ id ^ " trc x FVSYMBOL) (evalfold" ^ id ^ " x i))
    )
    (= FVUNKNOWN (evalfold" ^ id ^ " x i))
  ))
)
		" else if gamma = 1 then "
(define-fun evalfold" ^ id ^ " ((x Time) (i Time)) Fourvalue
  (evalb" ^ id ^ " trc x FVSYMBOL)
)" else raise (Failure ("gamma == 0"))
	in 
		let evalc id = "
(define-fun evalc" ^ id ^ " ((mt Time) (mtb Time)) (Pair Bool Fourvalue)
	(mk-pair (>= trc_size " ^ (string_of_int (gamma + t)) ^ ") (evalfold" ^ id ^ " (- mt 1) mtb ))
)
		" in comp1_append ^ comp2_append ^ (evalb id comp1 comp2) ^ (evalfold id) ^ (evalc id) ^ "
(define-fun computeUless" ^ id ^ "  ((mt Time) (mtb Time)) Threevalue
	(mapb3 (evalc" ^ id ^ " mt mtb))
)
	" in

(*
  synthesis of until less than or equal
*)
    let compute_until_lessequal id t gamma (comp1, comp1_append) (comp2, comp2_append) =
		let evalb id comp1 comp2 = "
(define-fun evalb" ^ id ^ "  ( (mk Trace) (mt Time) (v Fourvalue) ) Fourvalue
	(ite (= v FVSYMBOL) (evali "^ comp1 ^" "^ comp2 ^" ) v )
)
		" in

		let evalfold id = "
(declare-fun evalfold" ^ id ^ " ( (Time) (Time)) Fourvalue )
(assert (forall ((x Time) (i Time)) (=> (and (>= i 0) (and (>= x 0) (<= x "^ (string_of_int (gamma + t)) ^")) ) (ite
	(> x i)
	(= (evalb" ^ id ^ " trc x (evalfold" ^ id ^ " (- x 1) i )) (evalfold" ^ id ^ " x i )  )
	(= (evalb" ^ id ^ " trc x FVSYMBOL) (evalfold" ^ id ^ " x i))
   )))
)
		" in 
		let evalc id = "
(define-fun evalc" ^ id ^ " ((mt Time) (mtb Time)) (Pair Bool Fourvalue)
	(mk-pair (>= trc_size " ^ (string_of_int (gamma + t)) ^ ") (evalfold" ^ id ^ " mt mtb ))
)
		" in comp1_append ^ comp2_append ^ (evalb id comp1 comp2) ^ (evalfold id) ^ (evalc id) ^ "
(define-fun computeUlesseq" ^ id ^ "  ((mt Time) (mtb Time)) Threevalue
	(mapb3 (evalc" ^ id ^ " mt mtb))
)
	" in


(*
  synthesis of until equal
*)
	let compute_until_equal id t gamma (comp1, comp1_append) (comp2, comp2_append) =
		let evaliEq id = "
(define-fun evaliEq" ^ id ^ " ((b1 Threevalue) (b2 Threevalue) (mt Time) (mtb Time)) Fourvalue
	(ite (>= mt (+ mtb "^ string_of_int gamma ^")) (mapb4 b2) (ite (= b1 TVTRUE) FVSYMBOL (mapb4 b1) ) )
)
	    " in

		let evalb id comp1 comp2 = "
(define-fun evalb" ^ id ^ "  ( (mk Trace) (mt Time) (mtb Time) (v Fourvalue) ) Fourvalue
	(ite (= v FVSYMBOL) (evaliEq" ^ id ^ " "^ comp1 ^" "^ comp2 ^" mt mtb ) v )
)
		" in

		let evalfold id = "
(declare-fun evalfold" ^ id ^ " ( (Time) (Time)) Fourvalue )
(assert (forall ((x Time) (i Time)) (=> (and (>= i 0) (and (>= x 0) (<= x "^ (string_of_int (gamma + t)) ^")) ) (ite
    (> x i)
	(= (evalb" ^ id ^ " trc x i (evalfold" ^ id ^ " (- x 1) i )) (evalfold" ^ id ^ " x i )  )
	(= (evalb" ^ id ^ " trc x i FVSYMBOL) (evalfold" ^ id ^ " x i))
   )))
)
		" in 
		let evalc id = "
(define-fun evalc" ^ id ^ " ((mt Time) (mtb Time)) (Pair Bool Fourvalue)
	(mk-pair (>= trc_size " ^ (string_of_int (gamma + t)) ^ ") (evalfold" ^ id ^ " mt mtb ))
)
		" in comp1_append ^ comp2_append ^ evaliEq id ^ (evalb id comp1 comp2) ^ (evalfold id) ^ (evalc id) ^ "
(define-fun computeUequal" ^ id ^ "  ((mt Time) (mtb Time)) Threevalue
	(mapb3 (evalc" ^ id ^ " mt mtb))
)
	" in
(*
  END synthesis of until equal
*)

	let duration id t dt formula =
		let indicator id = "
(define-fun indicator"^ id ^" ((mk Trace) (mt Time)) Int
	(ite (= "^ formula ^" TVTRUE) 1 0)
)
		" in
		let evaleta id =
"
(declare-fun evaleta"^ id ^" ((Time) (Time)) Int)
(assert (forall ((x Time) (i Time)) (=> (and (>= x 0) (<= x (+ "^ (string_of_int t) ^" "^ dt ^") )) (ite
	(and (and (< x (+ "^ (string_of_int t) ^" "^ dt ^") ) (>= i 0)) (> x i))
	(= (evaleta"^ id ^" x i) (+ (evaleta"^ id ^" (- x 1) i) (indicator"^ id ^" trc x) ))
	(= (evaleta"^ id ^" x i) (indicator"^ id ^" trc x) )
	)))
)
		" in
	("(computeduration"^ id ^" (+ mt "^ dt ^") mt)", (indicator id) ^ (evaleta id) ^"
(define-fun computeduration"^ id ^" ((mt Time) (mtb Time)) Duration
	(evaleta"^ id ^" (- mt 1) mtb)
)
	")
	in


(* unfold formula *)
let rec synth_smtlib_tm t term helper =
  match term with
    | Constant value       -> (string_of_int(int_of_float value), "")
    | Duration (di,phi)    -> let idx = get_duration_counter helper in
                              let tr_out1, tr_out2 = synth_smtlib_tm t di helper in
    						  let sf_out1, sf_out2 = synth_smtlib_fm t phi helper in
    						  let dur_out1, dur_out2 = duration (string_of_int idx) t tr_out1 sf_out1 in
    						  (dur_out1, tr_out2^sf_out2^dur_out2)
    | FPlus (tr1,tr2)      -> ("", "")
    | FTimes (tr1,tr2)     -> ("", "")
    | _                    -> raise (Failure ("synth_smtlib_tm: bad term "^( Sexp.to_string_hum (sexp_of_rmtld3_tm term))))
and synth_smtlib_fm t formula helper =
  match formula with
    | True()                  -> ("TVTRUE","")
    | Prop p                  -> let tbl = get_proposition_hashtbl helper in
                                 let counter = get_proposition_counter helper in 
                                 let val1,val2 = try (Hashtbl.find tbl p,"") with Not_found -> set_proposition_two_way_map p counter helper; (counter, compute_proposition (string_of_int counter)) in
                                 ("(computeprop"^ (string_of_int val1) ^" mk mt)", val2)

    | Not sf                  -> let sf_out1, sf_out2 = synth_smtlib_fm t sf helper in
    							 ("(tvnot "^ sf_out1 ^" )", sf_out2)

    | Or (sf1, sf2)           -> let sf1_out1, sf1_out2 = synth_smtlib_fm t sf1 helper in
    							 let sf2_out1, sf2_out2 = synth_smtlib_fm t sf2 helper in
    							 ("(tvor "^ sf1_out1 ^" "^ sf2_out1 ^")", sf1_out2^sf2_out2)

    | Until (gamma, sf1, sf2) -> (*let range = (List.range itv_low `To itv_upp )  in*)
    							 let idx = get_until_counter helper in
    							 let sf1_out1, sf1_out2 = (synth_smtlib_fm (t + (int_of_float gamma)) sf1 helper) in
    							 let sf2_out1, sf2_out2 = (synth_smtlib_fm (t + (int_of_float gamma)) sf2 helper) in
    	 					     (
    								"(computeUless!" ^ (string_of_int idx) ^" (+ mt "^ (string_of_int (int_of_float gamma)) ^") mt )"
    							  ,
							    	(compute_until_less
							    		("!"^(string_of_int idx))
							    		t
							    		(int_of_float gamma)
							    		(sf1_out1, sf1_out2)
							    		(sf2_out1, sf2_out2)
							    	)
								 )
	| Until_leq (gamma,sf1,sf2)-> let idx = get_until_counter helper in
    							 let sf1_out1, sf1_out2 = (synth_smtlib_fm (t + (int_of_float gamma)) sf1 helper) in
    							 let sf2_out1, sf2_out2 = (synth_smtlib_fm (t + (int_of_float gamma)) sf2 helper) in
    	 					     (
    								"(computeUlesseq!" ^ (string_of_int idx) ^" (+ mt "^ (string_of_int (int_of_float gamma)) ^") mt )"
    							  ,
							    	(compute_until_lessequal
							    		("!"^(string_of_int idx))
							    		t
							    		(int_of_float gamma)
							    		(sf1_out1, sf1_out2)
							    		(sf2_out1, sf2_out2)
							    	)
								 )

	| Until_eq (gamma,sf1,sf2)-> let idx = get_until_counter helper in
	                             let sf1_out1, sf1_out2 = (synth_smtlib_fm (t + (int_of_float gamma)) sf1 helper) in
    							 let sf2_out1, sf2_out2 = (synth_smtlib_fm (t + (int_of_float gamma)) sf2 helper) in
                                 (
                                   "(computeUequal!" ^ (string_of_int idx) ^" (+ mt "^ (string_of_int (int_of_float gamma)) ^") mt )"
                                   ,
                                   (compute_until_equal
							    		("!"^(string_of_int idx))
							    		t
							    		(int_of_float gamma)
							    		(sf1_out1, sf1_out2)
							    		(sf2_out1, sf2_out2)
							    	)
                                 )

    | LessThan (tr1,tr2)      -> let tr1_out1, tr1_out2 = (synth_smtlib_tm t tr1 helper) in
    							 let tr2_out1, tr2_out2 = (synth_smtlib_tm t tr2 helper) in
    							 ("(tvlessthan "^ tr1_out1 ^" "^ tr2_out1 ^")", tr1_out2^tr2_out2)
    | _ -> raise (Failure ("synth_smtlib_fm: bad formula "^( Sexp.to_string_hum (sexp_of_rmtld3_fm formula)))) in


(* call the unfold function *)
(*let formula2 = Until(10., Prop("A"), Prop("B")) in
let formula = Until(10., Until(10., Prop("A"), Prop("B")), Prop("B")) in*)

let toassert,x = synth_smtlib_fm 0 formula helper in

common_types ^ map_macros ^ evali ^ new_trace ^ x ^ "
(define-fun allcheck  ((mk Trace) (mt Time)) Bool (= "^ toassert ^" TVTRUE) )

(assert (forall ((t Time)) (>= (select trc t) 0)  ))
(assert (allcheck trc 0) )
"
