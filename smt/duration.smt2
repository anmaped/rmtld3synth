(set-option :produce-models true)
(set-option :smt.qi.eager_threshold 100)
(set-option :smt.mbqi false)

; common definitions
(define-sort Proptype () Int)
(define-sort Time () Int)
(define-sort Trace () (Array Time Proptype) )

; (three value set + symbol) and three value set 
(declare-datatypes () ((Fourvalue FVTRUE FVFALSE FVUNKNOWN FVSYMBOL)))
(declare-datatypes () ((Threevalue TVTRUE TVFALSE TVUNKNOWN)))

; computation of proposition
(define-fun computeprop ( (mk Trace) (mt Time) (phi1 Proptype) ) Threevalue
	(ite (= (select mk mt) phi1) TVTRUE TVFALSE  ); [TODO] change that to native threevalue
)

; end common definitions


; initialization
;(declare-const traceinit Trace )


;(declare-const x Int)

(declare-const trcintermediate Trace)

; indicator function
(define-fun indicator ((mt Time)) Int
	(ite (and (= (computeprop trcintermediate mt 77) TVTRUE) (not (= (computeprop trcintermediate mt 66) TVTRUE))) 1 0)
)


;(assert (forall ((x Int)) (or (= (indicator x) 0) (= (indicator x) 1) ) ) )

; evaln function
(declare-fun evaln ((Time)) Int)
(assert (= 0 (evaln 0)))
(assert (forall ((x Int)) (=> (and (> x 0) (< x 20)) (= (evaln x ) (+ (evaln (- x 1)) (indicator x) ) ) )))

; computeduration function


(assert (= (evaln 10) 9 ))


;  #####################

;(define-sort proparray () (Array Int Bool) )
;(declare-const a1 proparray ) ; array identifying the truth value of the proposition one

;(define-fun eval ((y Bool)) Int
;  (if y 1 0)
;)

; define other function to eval array
;(define-fun eval_array ( (x Int) ) Int
;	(eval (select a1 x))
;)

;(declare-fun accumulator ( (Int) ) Int )
;(assert (= 0 (accumulator 0)))
;(assert (= (eval_array 0) (accumulator 1) ))
;(assert (forall ((x Int)) (=> (> x 1) (= (accumulator x) (+ (accumulator (- x 1) ) (eval_array  x) ) ) )) )

;(assert (= 11 (accumulator 10)))


;  #####################


;(apply (then simplify solve-eqs))

(check-sat)

(get-model)

(get-info :all-statistics)
