;(set-option :produce-models true)

;(set-logic AUFNIRA)
(set-option :smt.qi.eager_threshold 100)
(set-option :smt.mbqi false)


; proptype definition
(define-sort Proptype () Int)
(define-sort Time () Int)
(declare-datatypes (T1 T2) ((Pair (mk-pair (first T1) (second T2)))))

; trace definition
(define-sort Trace () (Array Time Proptype) )

; (three value set + symbol) and three value set 
(declare-datatypes () ((Fourvalue FVTRUE FVFALSE FVUNKNOWN FVSYMBOL)))
(declare-datatypes () ((Threevalue TVTRUE TVFALSE TVUNKNOWN)))

; lets define the conversion functions [TODO]
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



(declare-const intermediatetrc Trace ) ; array identifying the truth value of the proposition


; computation of proposition
(define-fun computeprop ( (mk Trace) (mt Time) (phi1 Proptype) ) Threevalue
	(ite (= (select mk mt) phi1) TVTRUE TVFALSE  ); [TODO] change that to native threevalue
)


; declare evali
(define-fun evali ((b1 Threevalue) (b2 Threevalue)) Fourvalue
	(ite (not (= b2 TVFALSE)) (mapb4 b2) (ite (and (not (= b1 TVTRUE)) (= b2 TVFALSE)) (mapb4 b1) FVSYMBOL ) )
)

; evalb is defined for two propositions 1,2 (this is the replacement phase)
(define-fun evalb ( (mk Trace) (mt Time) (v Fourvalue) ) Fourvalue
	(ite (= v FVSYMBOL) (evali (computeprop mk mt 1) (computeprop mk mt 2) ) v )
)


; declared variables
(declare-const gamma Time)
(declare-const trace_size Time)


; this function receive a model containing a trace, and one subtrace
(declare-fun evalfold ( (Trace) (Time) (Time)) Fourvalue )
;(assert (= (evalb intermediatetrc 0 FVSYMBOL) (evalfold intermediatetrc 0 )) )
;(assert (forall ((x Time)) (=> (> x 0) (= (evalb intermediatetrc x (evalfold intermediatetrc (- x 1) )) (evalfold intermediatetrc x )  ))) )


(assert (forall ((x Time) (i Time)) (ite
	(and (and (< x 12) (and (>= x 0) (>= i 0))) (> x i))
	(= (evalb intermediatetrc x (evalfold intermediatetrc (- x 1) i )) (evalfold intermediatetrc x i )  )
	(= (evalb intermediatetrc i FVSYMBOL) (evalfold intermediatetrc i i))
   )) )

;(declare-fun evalfold ( (Time) (Time) ) Fourvalue )
;(assert (forall ((t Time) (i Time)) 
;(ite
;	(<= t i)
;	(= (evalb intermediatetrc i FVSYMBOL) (evalfold t i ))
;	(= (evalb intermediatetrc t (evalfold (- t 1) i )) (evalfold t i )  )
;) ))

;(define-fun-rec evalfold ((mk Trace) (mt Time) ) Fourvalue
;	(ite (= mt 0)
;		(evalb mk 0 FVSYMBOL)
;		(ite (> mt 0) (evalb mk mt (evalfold mk (- mt 1) )) FVFALSE )
;	)
;)

; let's define the gamma of the until operator [FIXED]
(assert (= gamma 10))



; lets define the evalc
(define-fun evalc ((mk Trace) (mt Time) (bt Time)) (Pair Bool Fourvalue) 
	(mk-pair (<= trace_size gamma) (evalfold mk mt bt))
)

; let's relate trace_size with real trace size [TODO]


; computeUless definition
(define-fun computeUless ((mk Trace) (mt Time) (bt Time)) Threevalue
	(mapb3 (evalc mk mt bt))
)


; define trace
(declare-const traceinit Trace )

(declare-const t Time)
(assert (= t 1))
(assert (= (computeUless traceinit gamma t) TVTRUE) )

; both arrays are equal (parameterized array for evalfold)
(assert (forall ((x Time)) (= (select traceinit x )  (select intermediatetrc x )) ) )



; several optional claims
; ########################

; array of twos
;(assert (forall ((x Time)) (= (select traceinit x )  2) ) )


; until claim
(assert (forall ((x Time)) (=> (not (= x 4)) (= (select traceinit x )  1) ) ) )
(assert (= (select traceinit 4) 2) )


; ########################

(check-sat)

(get-model)

(get-info :all-statistics)

