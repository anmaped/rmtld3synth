
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
	
(define-fun evali ((b1 Threevalue) (b2 Threevalue)) Fourvalue
	(ite (not (= b2 TVFALSE)) (mapb4 b2) (ite (and (not (= b1 TVTRUE)) (= b2 TVFALSE)) (mapb4 b1) FVSYMBOL ) )
)
	
(declare-const trc Trace )
(declare-const trc_size Time)
	
(define-fun computeprop1 ( (mk Trace) (mt Time) (phi1 Proptype) ) Threevalue
	(ite (= (select mk mt) phi1) TVTRUE TVFALSE  );
)
	
(define-fun computeprop2 ( (mk Trace) (mt Time) (phi1 Proptype) ) Threevalue
	(ite (= (select mk mt) phi1) TVTRUE TVFALSE  );
)
	
(define-fun computeprop3 ( (mk Trace) (mt Time) (phi1 Proptype) ) Threevalue
	(ite (= (select mk mt) phi1) TVTRUE TVFALSE  );
)
	
(define-fun evalb!2  ( (mk Trace) (mt Time) (v Fourvalue) ) Fourvalue
	(ite (= v FVSYMBOL) (evali (tvor (computeprop1 mk mt 1) (computeprop2 mk mt 2)) (computeprop3 mk mt 3) ) v )
)
		
(declare-fun evalfold!2 ( (Time) (Time)) Fourvalue )
(assert (forall ((x Time) (i Time)) (=> (and (< x 10) (>= x 0)) (ite
	(and (and (< x 20) (>= i 0)) (> x i))
	(= (evalb!2 trc x (evalfold!2 (- x 1) i )) (evalfold!2 x i )  )
	(= (evalb!2 trc x FVSYMBOL) (evalfold!2 x i))
   )) ))
		
(define-fun evalc!2 ((mt Time) (mtb Time)) (Pair Bool Fourvalue)
	(mk-pair (<= trc_size 10) (evalfold!2 (- mt 1) mtb ))
)
		
(define-fun computeUless!2  ((mt Time) (mtb Time)) Threevalue
	(mapb3 (evalc!2 mt mtb))
)
	
(define-fun evalb!1  ( (mk Trace) (mt Time) (v Fourvalue) ) Fourvalue
	(ite (= v FVSYMBOL) (evali (computeUless!2 10 10) (computeprop3 mk mt 3) ) v )
)
		
(declare-fun evalfold!1 ( (Time) (Time)) Fourvalue )
(assert (forall ((x Time) (i Time)) (=> (and (< x 10) (>= x 0)) (ite
	(and (and (< x 10) (>= i 0)) (> x i))
	(= (evalb!1 trc x (evalfold!1 (- x 1) i )) (evalfold!1 x i )  )
	(= (evalb!1 trc x FVSYMBOL) (evalfold!1 x i))
   )) ) )
		
(define-fun evalc!1 ((mt Time) (mtb Time)) (Pair Bool Fourvalue)
	(mk-pair (<= trc_size 10) (evalfold!1 (- mt 1) mtb ))
)
		
(define-fun computeUless!1  ((mt Time) (mtb Time)) Threevalue
	(mapb3 (evalc!1 mt mtb))
)
	
(define-fun indicator ((mk Trace) (mt Time)) Int
	(ite (= (computeprop1 mk mt 3) TVTRUE) 1 0)
)
		
(declare-fun evaleta ((Time) (Time)) Int)
(assert (forall ((xd Time) (id Time)) (ite
	(and (and (< xd 14 ) (>= id 0)) (> xd id))
	(= (evaleta xd id) (+ (evaleta (- xd 1) id) (indicator trc xd) ))
	(= (evaleta xd id) 0 )
	))
)
		
(define-fun computeduration ((mt Time) (mtb Time)) Duration
	(evaleta mt mtb)
)
	
(assert (= (computeUless!1 10 0) TVTRUE) )
(assert (= (tvlessthan 10 (computeduration 9 0)) TVTRUE) )


(check-sat-using smt)
(get-model)

(get-info :all-statistics)

