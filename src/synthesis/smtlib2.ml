open Batteries
open Sexplib
open Sexplib.Conv
open Dolmen
open Rmtld3
open Helper

type body = Term.t * Statement.t list

module SS = Set.Make(String)

let recursive_unrolling = ref false

let recursive_unrolling_depth = ref 0

let assume_unary_sequence = ref false

type solvers = Z3 | CVC4 | UNKNOWN

let solver = ref UNKNOWN

let isZ3SolverEnabled () = !solver = Z3

let isCvc4SolverEnabled () = !solver = CVC4

let enable_recursive_unrolling () = recursive_unrolling := true

let set_solver (slv : solvers) = solver := slv

let free_variables_set = ref SS.empty
let lst = ref []

let add a = lst := a :: !lst

let add_l l = lst := l @ !lst

let set_option lst a b =
  lst :=
    Statement.set_option
      (Term.colon (Term.const (Id.mk Id.Attr a)) (Term.const (Id.mk Id.Term b)))
    :: !lst

let set_info lst a b =
  lst :=
    Statement.set_info
      (Term.colon (Term.const (Id.mk Id.Attr a)) (Term.const (Id.mk Id.Term b)))
    :: !lst

let set_logic lst a = lst := Statement.set_logic a :: !lst

let define_sort lst id args body =
  lst := Statement.type_def id args body :: !lst

let define_fun lst id args ty_rec body =
  lst := Statement.fun_def id args ty_rec body :: !lst

let define_datatypes lst a = lst := Statement.datatypes a :: !lst

let check_sat lst = lst := Statement.check_sat [] :: !lst

let get_model lst = lst := Statement.get_model () :: !lst

let get_info lst ifo = lst := Statement.get_info ifo :: !lst

let declare_const lst id typ = lst := Statement.decl id typ :: !lst

let declare_fun lst id l typ = lst := Statement.fun_decl id l typ :: !lst

let assert_ lst a = lst := Statement.assert_ a :: !lst

let f_equal a b = Term.apply (Term.const (Id.mk Id.Term "=")) [a; b]

let ite a b c = Term.apply (Term.const (Id.mk Id.Term "ite")) [a; b; c]

let f_const_term a = Term.const (Id.mk Id.Term a)

let f_const_sort a = Term.const (Id.mk Id.Sort a)

let f_true = f_const_term "true"

let f_tvtrue = f_const_term "TVTRUE"

let f_fvtrue = f_const_term "FVTRUE"

let f_not a = Term.apply (Term.const (Id.mk Id.Term "not")) [a]

let f_tvnot a = Term.apply (Term.const (Id.mk Id.Term "tvnot")) [a]

let f_or a b = Term.apply (Term.const (Id.mk Id.Term "or")) [a; b]

let f_tvor a b = Term.apply (Term.const (Id.mk Id.Term "tvor")) [a; b]

let f_and a b = Term.apply (Term.const (Id.mk Id.Term "and")) [a; b]

let f_tvand a b = f_tvnot (f_tvor (f_tvnot a) (f_tvnot b) )

let f_implies a b = Term.apply (Term.const (Id.mk Id.Term "=>")) [a; b]

let f_less a b = Term.apply (Term.const (Id.mk Id.Term "<")) [a; b]

let f_leq a b = Term.apply (Term.const (Id.mk Id.Term "<=")) [a; b]

let f_geq a b = Term.apply (Term.const (Id.mk Id.Term ">=")) [a; b]

let f_sum a b = Term.apply (Term.const (Id.mk Id.Term "+")) [a; b]

let f_minus a b = Term.apply (Term.const (Id.mk Id.Term "-")) [a; b]

let f_times a b = Term.apply (Term.const (Id.mk Id.Term "*")) [a; b]

let empty_body a = (a, [])

let cartesian l l' =
  List.concat (List.map (fun e -> List.map (fun e' -> (e, e')) l') l)

(*
	Synthesis
*)

(* parameterized synthesis functions for until operators *)
let evalfold_param id =
  let evalfold id =
    (* (declare-fun evalfold" ^ id ^ " (Time Time) Fourvalue ) *)
    declare_fun lst
      (Id.mk Id.Term ("evalfold" ^ id))
      [Term.const (Id.mk Id.Sort "Time"); Term.const (Id.mk Id.Sort "Time")]
      (Term.const (Id.mk Id.Term "Fourvalue")) ;

    if !recursive_unrolling then (
      (* unrooling the recursion just in case (speedup) *)
      let enumeration = List.of_enum (0 -- !recursive_unrolling_depth) in
      let lst_all_comb = cartesian enumeration enumeration in
      (* (assert (forall ((x Time) (i Time)) (implies (not (and (and (<= 0 x)  (<= x max_depth) ) (and (<= 0 i)  (<= i max_depth) ) ) )  (= (evalfold!id x i) FVTRUE ) )  ) ) *)
      (* this assert closes the unroll search space with upper and lower bound for freevariables x and i *)
      assert_ lst
        (Term.forall
          [ Term.colon
              (Term.const (Id.mk Id.Term "x"))
              (Term.const (Id.mk Id.Sort "Time"))
          ; Term.colon
              (Term.const (Id.mk Id.Term "i"))
              (Term.const (Id.mk Id.Sort "Time")) ]
          (f_implies
            (f_not
              (f_and
                (f_and
                  (f_leq
                    (f_const_term "0")
                    (f_const_term "x")
                  )
                  (f_leq
                    (f_const_term "x")
                    (f_const_term (string_of_int !recursive_unrolling_depth))
                  )
                )
                (f_and
                  (f_leq
                    (f_const_term "0")
                    (f_const_term "i")
                  )
                  (f_leq
                    (f_const_term "i")
                    (f_const_term (string_of_int !recursive_unrolling_depth))
                  )
                )
              )
            )
            (f_equal
              (Term.apply
                (Term.const (Id.mk Id.Term ("evalfold" ^ id)) )
                [ Term.const (Id.mk Id.Term "x")
                      ; Term.const (Id.mk Id.Term "i") ]
              )
              f_fvtrue
            )
          )
        ) ;
      List.fold_left
        (fun a (x, i) ->
          if x > i then
            (* (assert (= (evalb" ^ id ^ " trc "^ string_of_int x ^" "^ string_of_int i ^" (evalfold" ^ id ^ " (- "^ string_of_int x ^" 1) "^ string_of_int i ^" )) (evalfold" ^ id ^ " "^ string_of_int x ^" "^ string_of_int i ^" ) ) ) *)
            assert_ lst
              (f_equal
                 (Term.apply
                    (Term.const (Id.mk Id.Term ("evalb" ^ id)))
                    [ Term.const (Id.mk Id.Term "trc")
                    ; Term.const (Id.mk Id.Term (string_of_int x))
                    ; Term.const (Id.mk Id.Term (string_of_int i))
                    ; Term.apply
                        (Term.const (Id.mk Id.Term ("evalfold" ^ id)))
                        [ f_minus
                            (Term.const (Id.mk Id.Term (string_of_int x)))
                            (Term.const (Id.mk Id.Term "1"))
                        ; Term.const (Id.mk Id.Term (string_of_int i)) ] ])
                 (Term.apply
                    (Term.const (Id.mk Id.Term ("evalfold" ^ id)))
                    [ Term.const (Id.mk Id.Term (string_of_int x))
                    ; Term.const (Id.mk Id.Term (string_of_int i)) ]))
          else
            (* (assert (= (evalb" ^ id ^ " trc "^ string_of_int x ^" "^ string_of_int i ^" FVSYMBOL) (evalfold" ^ id ^ " "^ string_of_int x ^" "^ string_of_int i ^")) ) *)
            assert_ lst
              (f_equal
                 (Term.apply
                    (Term.const (Id.mk Id.Term ("evalb" ^ id)))
                    [ Term.const (Id.mk Id.Term "trc")
                    ; Term.const (Id.mk Id.Term (string_of_int x))
                    ; Term.const (Id.mk Id.Term (string_of_int i))
                    ; Term.const (Id.mk Id.Term "FVSYMBOL") ])
                 (Term.apply
                    (Term.const (Id.mk Id.Term ("evalfold" ^ id)))
                    [ Term.const (Id.mk Id.Term (string_of_int x))
                    ; Term.const (Id.mk Id.Term (string_of_int i)) ])) )
        () lst_all_comb ;
      () )
    else (
      (*
				(assert (forall ((x Time) (i Time))
				  (ite (> x i)
					  (= (evalb" ^ id ^ " trc x i (evalfold" ^ id ^ " (- x 1) i )) (evalfold" ^ id ^ " x i ) )
					  (= (evalb" ^ id ^ " trc x i FVSYMBOL) (evalfold" ^ id ^ " x i))
				  )
				))
			*)
      assert_ lst
        (Term.forall
           [ Term.colon
               (Term.const (Id.mk Id.Term "x"))
               (Term.const (Id.mk Id.Sort "Time"))
           ; Term.colon
               (Term.const (Id.mk Id.Term "i"))
               (Term.const (Id.mk Id.Sort "Time")) ]
           (ite
              (f_less
                 (Term.const (Id.mk Id.Term "i"))
                 (Term.const (Id.mk Id.Term "x")))
              (f_equal
                 (Term.apply
                    (Term.const (Id.mk Id.Term ("evalb" ^ id)))
                    [ Term.const (Id.mk Id.Term "trc")
                    ; Term.const (Id.mk Id.Term "x")
                    ; Term.const (Id.mk Id.Term "i")
                    ; Term.apply
                        (Term.const (Id.mk Id.Term ("evalfold" ^ id)))
                        [ f_minus
                            (Term.const (Id.mk Id.Term "x"))
                            (Term.const (Id.mk Id.Term "1"))
                        ; Term.const (Id.mk Id.Term "i") ] ])
                 (Term.apply
                    (Term.const (Id.mk Id.Term ("evalfold" ^ id)))
                    [ Term.const (Id.mk Id.Term "x")
                    ; Term.const (Id.mk Id.Term "i") ]))
              (f_equal
                 (Term.apply
                    (Term.const (Id.mk Id.Term ("evalb" ^ id)))
                    [ Term.const (Id.mk Id.Term "trc")
                    ; Term.const (Id.mk Id.Term "x")
                    ; Term.const (Id.mk Id.Term "i")
                    ; Term.const (Id.mk Id.Term "FVSYMBOL") ])
                 (Term.apply
                    (Term.const (Id.mk Id.Term ("evalfold" ^ id)))
                    [ Term.const (Id.mk Id.Term "x")
                    ; Term.const (Id.mk Id.Term "i") ])))) ;
      () )
  in
  evalfold id

let dummy_tuple = (Term.const (Id.mk Id.Term "dummy"), [])

let synth_tm_constant value helper =
  empty_body (Term.apply (f_const_term "dsome") [(Term.const (Id.mk Id.Term (string_of_int (int_of_float value))))] )

let synth_tm_variable name helper =
  if not (SS.exists (fun s -> s = name) !free_variables_set) then ( free_variables_set := SS.add name !free_variables_set; declare_const lst (Id.mk Id.Term name) (Term.const (Id.mk Id.Sort "Real")) ; ) ;
  empty_body (Term.apply (f_const_term "dsome") [(Term.const (Id.mk Id.Term name))] )

let synth_tm_duration (tm_call, tm_body) (fm_call, fm_body) helper =
  let idx = get_duration_counter helper in
  let freevariable = "v_dt!" ^ string_of_int idx in
  declare_const lst (Id.mk Id.Term freevariable) (Term.const (Id.mk Id.Sort "Duration")) ;
  if !assume_unary_sequence then
  assert_ lst ( f_equal (Term.const (Id.mk Id.Term freevariable)) (tm_call) )
else
  (
  
  (if !recursive_unrolling then ( assert_ lst (f_and
    ( f_less (Term.apply (f_const_term "dval") [f_const_term freevariable]) (f_const_term (string_of_int !recursive_unrolling_depth)) )
    ( f_less (f_const_term "0") (Term.apply (f_const_term "dval") [f_const_term freevariable]) ) ) ;
    (* (assert (forall ((i Index)) (<= 0 (select trc_time i) ) ) ) *)
    assert_ lst (
Term.forall
             [ Term.colon (f_const_term "i") (f_const_sort "Index") ]
             (
              (f_and (f_less (Term.apply (f_const_term "select") [f_const_sort "trc_time"; f_const_sort "i"]) (f_const_sort (string_of_int 100)) ) )
               (f_less (f_const_sort "0") (Term.apply (f_const_term "select") [f_const_sort "trc_time"; f_const_sort "i"]) ) )
    ) ;
  ) ); ) ;

  let duration id fm =
    let l_lst = ref [] in
    let indicator id =
      (*
				(define-fun indicator"^ id ^" ((mk Trace) (mt Time)) Int
					(ite (= "^ formula ^" TVTRUE) 1 0)
				)
			*)
      define_fun l_lst
        (* id *)
        (Id.mk Id.Term ("indicator" ^ id))
        (* arguments *)
        [ Term.colon (f_const_term "mk") (f_const_sort "Trace")
        ; Term.colon (f_const_term "mt") (f_const_sort "Time") ]
        (* return type *)
        (Term.const (Id.mk Id.Sort "Int"))
        (* body *)
        (ite
           (f_equal fm (f_const_term "TVTRUE"))
           (f_const_term "1") (f_const_term "0"))
    in
    let evaleta id =
      (* (declare-fun evaleta"^ id ^" ((Time) (Time)) Duration) *)
      declare_fun l_lst
        (Id.mk Id.Term ("evaleta" ^ id))
        [f_const_sort "Time"; f_const_sort "Time"]
        (Term.const (Id.mk Id.Term "Duration")) ;
      if !recursive_unrolling then (
        (* unrooling the recursion just in case (speedup) *)
        let enumeration = List.of_enum (0 -- !recursive_unrolling_depth) in
        let lst_all_comb = cartesian enumeration enumeration in
        (* (assert (forall ((x Time) (i Time)) (implies (not (and (and (<= 0 x)  (<= x max_depth) ) (and (<= 0 i)  (<= i max_depth) ) ) )  (= evaleta!id x i) dnone ) )  ) ) *)
        (* this assert closes the unroll search space with upper and lower bound for freevariables x and i *)
        assert_ l_lst
          (Term.forall
            [ Term.colon
                (Term.const (Id.mk Id.Term "x"))
                (Term.const (Id.mk Id.Sort "Time"))
            ; Term.colon
                (Term.const (Id.mk Id.Term "i"))
                (Term.const (Id.mk Id.Sort "Time")) ]
            (f_implies
              (f_not
                (f_and
                  (f_and
                    (f_leq
                      (f_const_term "0")
                      (f_const_term "x")
                    )
                    (f_leq
                      (f_const_term "x")
                      (f_const_term (string_of_int !recursive_unrolling_depth))
                    )
                  )
                  (f_and
                    (f_leq
                      (f_const_term "0")
                      (f_const_term "i")
                    )
                    (f_leq
                      (f_const_term "i")
                      (f_const_term (string_of_int !recursive_unrolling_depth))
                    )
                  )
                )
              )
              (f_equal
                (Term.apply
                  (Term.const (Id.mk Id.Term ("evaleta" ^ id)) )
                  [ Term.const (Id.mk Id.Term "x")
                        ; Term.const (Id.mk Id.Term "i") ]
                )
                (f_const_term "dnone")
              )
            )
          ) ;
        List.fold_left
          (fun a (x, i) ->
            if x > i then
              (* (assert (= (evaleta"^ id ^" "^ string_of_int x ^" "^ string_of_int i ^") (+ (evaleta"^ id ^" (- "^ string_of_int x ^" 1) "^ string_of_int i ^") (indicator"^ id ^" trc "^ string_of_int x ^") )) ) *)
              assert_ l_lst
                (f_equal
                   (Term.apply (f_const_term "dval")
                      [ Term.apply
                          (f_const_term ("evaleta" ^ id))
                          [ f_const_term (string_of_int x)
                          ; f_const_term (string_of_int i) ] ])
                   (f_sum
                      (Term.apply (f_const_term "dval")
                         [ Term.apply
                             (f_const_term ("evaleta" ^ id))
                             [ f_minus
                                 (f_const_term (string_of_int x))
                                 (f_const_term "1")
                             ; f_const_term (string_of_int i) ] ])
                      (f_times (Term.apply
                         (f_const_term ("indicator" ^ id))
                         [f_const_term "trc"; f_const_term (string_of_int x)])
                         (if !assume_unary_sequence then f_const_term "1" else (Term.apply (f_const_term "select") [f_const_term "trc_time"; f_const_term (string_of_int x) ] ) )
                      ) ))
            else
              (* (assert (= (evaleta"^ id ^" "^ string_of_int x ^" "^ string_of_int i ^") (indicator"^ id ^" trc "^ string_of_int x ^") ) ) *)
              assert_ l_lst
                (f_equal
                   (Term.apply (f_const_term "dval")
                      [ Term.apply
                          (f_const_term ("evaleta" ^ id))
                          [ f_const_term (string_of_int x)
                          ; f_const_term (string_of_int i) ] ])
                   (f_times
                    (Term.apply
                      (f_const_term ("indicator" ^ id))
                      [f_const_term "trc"; f_const_term (string_of_int x)])
                    (if !assume_unary_sequence then f_const_term "1" else (Term.apply (f_const_term "select") [f_const_term "trc_time"; f_const_term (string_of_int x) ] ) )
                   ) ) )
          () lst_all_comb ;
        () )
      else (
        (*
					(assert (forall ((x Time) (i Time)) (=> (and (>= x 0) (<= x (+ "^ (string_of_int t) ^" "^ dt ^") )) (ite
						(and (and (< x (+ "^ (string_of_int t) ^" "^ dt ^") ) (>= i 0)) (> x i))
						(= (evaleta"^ id ^" x i) (+ (evaleta"^ id ^" (- x 1) i) (indicator"^ id ^" trc x) ))
						(= (evaleta"^ id ^" x i) (indicator"^ id ^" trc x) )
						)))
					)
				*)
        assert_ l_lst
          (Term.forall
             [ Term.colon (f_const_term "x") (f_const_sort "Time")
             ; Term.colon (f_const_term "i") (f_const_sort "Time") ]
             (f_implies
                (*(and (>= x 0) (<= x (+ "^ (string_of_int t) ^" "^ dt ^") ))*)
                (f_geq (f_const_term "x") (f_const_term "0"))
                (* ignores upper bound *)
                (ite
                   (* (and (and (< x (+ "^ (string_of_int t) ^" "^ dt ^") ) (>= i 0)) (> x i)) *)
                   (* ignores upper bound *)
                   (f_and
                      (f_geq (f_const_term "i") (f_const_term "0"))
                      (f_less (f_const_term "i") (f_const_term "x")))
                   (* (= (evaleta"^ id ^" x i) (+ (evaleta"^ id ^" (- x 1) i) (indicator"^ id ^" trc x) )) *)
                   (f_equal
                      (f_sum
                         (Term.apply (f_const_term "dval")
                            [ Term.apply
                                (f_const_term ("evaleta" ^ id))
                                [ f_minus (f_const_term "x") (f_const_term "1")
                                ; f_const_term "i" ] ])
                         (Term.apply
                            (f_const_term ("indicator" ^ id))
                            [f_const_term "trc"; f_const_term "x"]))
                      (Term.apply (f_const_term "dval")
                         [ Term.apply
                             (f_const_term ("evaleta" ^ id))
                             [f_const_term "x"; f_const_term "i"] ]))
                   (* (= (evaleta"^ id ^" x i) (indicator"^ id ^" trc x) ) *)
                   (f_equal
                      (Term.apply (f_const_term "dval")
                         [ Term.apply
                             (f_const_term ("evaleta" ^ id))
                             [f_const_term "x"; f_const_term "i"] ])
                      (Term.apply
                         (f_const_term ("indicator" ^ id))
                         [f_const_term "trc"; f_const_term "x"]))))) ;
        () )
    in
    indicator id ;
    evaleta id ;
    (*
			(define-fun duration_op"^ id ^" ((mt Time) (mtb Time)) Duration
				(and (>= trc_size mt) (evaleta"^ id ^" (- mt 1) mtb) )
			)
		*)
    define_fun l_lst
      (* id *)
      (Id.mk Id.Term ("duration_op" ^ id))
      (* arguments *)
      [ Term.colon
          (Term.const (Id.mk Id.Term "mt"))
          (Term.const (Id.mk Id.Sort "Time"))
      ; Term.colon
          (Term.const (Id.mk Id.Term "mtb"))
          (Term.const (Id.mk Id.Sort "Time")) ]
      (* return type *)
      (Term.const (Id.mk Id.Sort "Duration"))
      (* body *)
      (ite
         (f_and
            (f_geq
              (Term.const (Id.mk Id.Term "trc_size"))
              (Term.const (Id.mk Id.Term "mt")))
            (
              (* one assumption *)
              if not !assume_unary_sequence then f_equal (Term.apply (f_const_term "dval") [tm_call]) (f_minus (Term.apply (f_const_term "mapt") [f_const_term "mt"]) (Term.apply (f_const_term "mapt") [f_const_term "mtb"]) ) else f_true
            )
         )
         (Term.apply
            (Term.const (Id.mk Id.Term ("evaleta" ^ id)))
            [ f_minus
                (Term.const (Id.mk Id.Term "mt"))
                (Term.const (Id.mk Id.Term "1"))
            ; Term.const (Id.mk Id.Term "mtb") ])
         (Term.const (Id.mk Id.Term "dnone"))) ;
    (* this symbol is not correct; it should be the bottom_duration *)
    (* (duration_op"^ id ^" (+ mt "^ dt ^") mt) *)
    ( Term.apply
        (Term.const (Id.mk Id.Term ("duration_op" ^ id)))
        [ f_sum (Term.const (Id.mk Id.Term "mt")) (Term.apply (f_const_term "dval") [f_const_term freevariable])
        ; Term.const (Id.mk Id.Term "mt") ]
    , !l_lst (* list of asserts and intermediate definitions *) )
  in
  let dur_out1, dur_out2 =
    duration ("!" ^ string_of_int idx) fm_call
  in
  (dur_out1, tm_body @ fm_body @ dur_out2)

let synth_tm_plus cmptr1 cmptr2 helper =
  (Term.apply (Term.const (Id.mk Id.Term "dsome")) [f_sum (Term.apply (Term.const (Id.mk Id.Term "dval")) [fst cmptr1]) (Term.apply (Term.const (Id.mk Id.Term "dval")) [fst cmptr2])], snd cmptr1 @ snd cmptr2)

let synth_tm_times cmptr1 cmptr2 helper =
  (Term.apply (Term.const (Id.mk Id.Term "dsome")) [f_times (Term.apply (Term.const (Id.mk Id.Term "dval")) [fst cmptr1]) (Term.apply (Term.const (Id.mk Id.Term "dval")) [fst cmptr2])], snd cmptr1 @ snd cmptr2)

let synth_fm_true helper = empty_body (Term.const (Id.mk Id.Term "TVTRUE"))

let synth_fm_p p helper =
  empty_body
    (Term.apply
       (Term.const (Id.mk Id.Term "ev_prop"))
       [ Term.const (Id.mk Id.Term "mk")
       ; Term.const (Id.mk Id.Term "mt")
       ; Term.const (Id.mk Id.Term (string_of_int p)) ])

let synth_fm_not cmpfm helper =
  (Term.apply (Term.const (Id.mk Id.Term "tvnot")) [fst cmpfm], snd cmpfm)

let synth_fm_or cmpfm1 cmpfm2 helper =
  ( Term.apply (Term.const (Id.mk Id.Term "tvor")) [fst cmpfm1; fst cmpfm2]
  , snd cmpfm1 @ snd cmpfm2 )

let synth_fm_less cmptr1 cmptr2 helper =
  ( Term.apply
      (Term.const (Id.mk Id.Term "tvlessthan"))
      [fst cmptr1; fst cmptr2]
  , snd cmptr1 @ snd cmptr2 )

(*
  synthesis of U<
*)
let synth_fm_uless gamma sf1 sf2 helper =
  let idx = get_until_counter helper in
  let freevariable = "v_gamma!" ^ string_of_int idx in
  declare_const lst (Id.mk Id.Term freevariable) (Term.const (Id.mk Id.Sort "Index")) ;
  assert_ lst ( f_equal (Term.const (Id.mk Id.Term freevariable)) (Term.const (Id.mk Id.Term ( string_of_int (int_of_float gamma)) )) ) ;
  let until_less id (comp1, comp1_append) (comp2, comp2_append) =
    let evalb id =
      (*
				(define-fun evalb" ^ id ^ "  ( (mk Trace) (mt Time) (mtb Time) (v Fourvalue) ) Fourvalue
					(ite (= v FVSYMBOL) (evali "^ comp1 ^" "^ comp2 ^" ) v )
				)
			*)
      define_fun lst
        (* id *)
        (Id.mk Id.Term ("evalb" ^ id))
        (* arguments *)
        [ Term.colon
            (Term.const (Id.mk Id.Term "mk"))
            (Term.const (Id.mk Id.Sort "Trace"))
        ; Term.colon
            (Term.const (Id.mk Id.Term "mt"))
            (Term.const (Id.mk Id.Sort "Time"))
        ; Term.colon
            (Term.const (Id.mk Id.Term "mtb"))
            (Term.const (Id.mk Id.Sort "Time"))
        ; Term.colon
            (Term.const (Id.mk Id.Term "v"))
            (Term.const (Id.mk Id.Sort "Fourvalue")) ]
        (* return type *)
        (Term.const (Id.mk Id.Sort "Fourvalue"))
        (* body *)
        (ite
           (f_equal
              (Term.const (Id.mk Id.Term "v"))
              (Term.const (Id.mk Id.Term "FVSYMBOL")))
           (Term.apply (Term.const (Id.mk Id.Term "evali")) [comp1; comp2])
           (Term.const (Id.mk Id.Term "v")))
    in
    let evalfold id = evalfold_param id in
    let evalc id =
      (*
				(define-fun evalc" ^ id ^ " ((mt Time) (mtb Time) ) (Pair Bool Fourvalue)
					(mk-pair (<= trc_size (+ " ^ (string_of_int gamma) ^ " mtb ) ) (evalfold" ^ id ^ " (- mt 1) mtb ))
				)
			*)
      define_fun lst
        (* id *)
        (Id.mk Id.Term ("evalc" ^ id))
        (* arguments *)
        [ Term.colon
            (Term.const (Id.mk Id.Term "mt"))
            (Term.const (Id.mk Id.Sort "Time"))
        ; Term.colon
            (Term.const (Id.mk Id.Term "mtb"))
            (Term.const (Id.mk Id.Sort "Time")) ]
        (* return type *)
        (Term.apply
           (Term.const (Id.mk Id.Sort "Pair"))
           [ Term.const (Id.mk Id.Sort "Bool")
           ; Term.const (Id.mk Id.Sort "Fourvalue") ])
        (* body *)
        (Term.apply
           (Term.const (Id.mk Id.Term "mk-pair"))
           [ f_geq
               (f_sum
                  (Term.const (Id.mk Id.Term freevariable))
                  (Term.const (Id.mk Id.Term "mtb")))
               (Term.const (Id.mk Id.Term "trc_size"))
           ; Term.apply
               (Term.const (Id.mk Id.Term ("evalfold" ^ id)))
               [ f_minus
                   (Term.const (Id.mk Id.Term "mt"))
                   (Term.const (Id.mk Id.Term "1"))
               ; Term.const (Id.mk Id.Term "mtb") ] ])
    in
    evalb id ;
    evalfold id ;
    evalc id ;
    add_l comp1_append ;
    add_l comp2_append ;
    (*
			(define-fun until_less_op" ^ id ^ "  ((mt Time) (mtb Time) ) Threevalue
				(mapb3 (evalc" ^ id ^ " mt mtb ))
			)
		*)
    define_fun lst
      (* id *)
      (Id.mk Id.Term ("until_less_op" ^ id))
      (* arguments *)
      [ Term.colon
          (Term.const (Id.mk Id.Term "mt"))
          (Term.const (Id.mk Id.Sort "Time"))
      ; Term.colon
          (Term.const (Id.mk Id.Term "mtb"))
          (Term.const (Id.mk Id.Sort "Time")) ]
      (* return type *)
      (Term.const (Id.mk Id.Sort "Threevalue"))
      (* body *)
      (Term.apply
         (Term.const (Id.mk Id.Term "mapb3"))
         [ Term.apply
             (Term.const (Id.mk Id.Term ("evalc" ^ id)))
             [Term.const (Id.mk Id.Term "mt"); Term.const (Id.mk Id.Term "mtb")]
         ])
  in
  until_less ("!" ^ string_of_int idx) sf1 sf2 ;
  (* "(until_less_op!" ^ (string_of_int idx) ^" (+ mt "^ (string_of_int (int_of_float gamma)) ^") mt t )" *)
  ( Term.apply
      (Term.const (Id.mk Id.Term ("until_less_op!" ^ string_of_int idx)))
      [ f_sum
          (Term.const (Id.mk Id.Term "mt"))
          (Term.const (Id.mk Id.Term freevariable))
      ; Term.const (Id.mk Id.Term "mt") ]
  , []
    (* things are append in until_less; we don't need (snd sf1@snd sf2) *)
  )

let synth_fm_ev_eq gamma sf1 helper =
  let idx = get_until_counter helper in
  let freevariable = "v_gamma!" ^ string_of_int idx in
  declare_const lst (Id.mk Id.Term freevariable) (Term.const (Id.mk Id.Sort "Index")) ;
  assert_ lst ( f_equal (Term.const (Id.mk Id.Term freevariable)) (Term.const (Id.mk Id.Term ( string_of_int (int_of_float gamma)) )) ) ;
  let eventually_eq id (comp1, comp1_append) =
    let evalb id =
      (*
        (define-fun evalb" ^ id ^ "  ( (mk Trace) (mt Time) (mtb Time) (v Fourvalue) ) Fourvalue
          (ite (= v FVSYMBOL) (evali "^ comp1 ^" "^ comp2 ^" ) v )
        )
      *)
      define_fun lst
        (* id *)
        (Id.mk Id.Term ("evalb" ^ id))
        (* arguments *)
        [ Term.colon
            (Term.const (Id.mk Id.Term "mk"))
            (Term.const (Id.mk Id.Sort "Trace"))
        ; Term.colon
            (Term.const (Id.mk Id.Term "mt"))
            (Term.const (Id.mk Id.Sort "Time"))
        ; Term.colon
            (Term.const (Id.mk Id.Term "mtb"))
            (Term.const (Id.mk Id.Sort "Time"))
        ; Term.colon
            (Term.const (Id.mk Id.Term "v"))
            (Term.const (Id.mk Id.Sort "Fourvalue")) ]
        (* return type *)
        (Term.const (Id.mk Id.Sort "Fourvalue"))
        (* body *)
        (ite
           (f_equal
              (Term.const (Id.mk Id.Term "v"))
              (Term.const (Id.mk Id.Term "FVSYMBOL")))
           (Term.apply (Term.const (Id.mk Id.Term "evali")) [f_tvtrue; comp1])
           (Term.const (Id.mk Id.Term "v")))
    in
    let evalc id =
      (*
        (define-fun evalc" ^ id ^ " ((mt Time) (mtb Time) ) (Pair Bool Fourvalue)
          (mk-pair (<= trc_size (+ " ^ (string_of_int gamma) ^ " mtb ) ) (evalb" ^ id ^ " trc mt mtb FVSYMBOL ))
        )
      *)
      define_fun lst
        (* id *)
        (Id.mk Id.Term ("evalc" ^ id))
        (* arguments *)
        [ Term.colon
            (Term.const (Id.mk Id.Term "mt"))
            (Term.const (Id.mk Id.Sort "Time"))
        ; Term.colon
            (Term.const (Id.mk Id.Term "mtb"))
            (Term.const (Id.mk Id.Sort "Time")) ]
        (* return type *)
        (Term.apply
           (Term.const (Id.mk Id.Sort "Pair"))
           [ Term.const (Id.mk Id.Sort "Bool")
           ; Term.const (Id.mk Id.Sort "Fourvalue") ])
        (* body *)
        (Term.apply
           (Term.const (Id.mk Id.Term "mk-pair"))
           [ f_geq
               (f_sum
                  (Term.const (Id.mk Id.Term freevariable))
                  (Term.const (Id.mk Id.Term "mtb")))
               (Term.const (Id.mk Id.Term "trc_size"))
           ; Term.apply
               (Term.const (Id.mk Id.Term ("evalb" ^ id)))
               [ Term.const (Id.mk Id.Term "trc")
               ; Term.const (Id.mk Id.Term "mt")
               ; Term.const (Id.mk Id.Term "mtb")
               ; Term.const (Id.mk Id.Term "FVSYMBOL") ] ])
    in
    evalb id ;
    evalc id ;
    add_l comp1_append ;
    (*
      (define-fun eventually_op" ^ id ^ "  ((mt Time) (mtb Time) ) Threevalue
        (mapb3 (evalc" ^ id ^ " mt mtb ))
      )
    *)
    define_fun lst
      (* id *)
      (Id.mk Id.Term ("eventually_op" ^ id))
      (* arguments *)
      [ Term.colon
          (Term.const (Id.mk Id.Term "mt"))
          (Term.const (Id.mk Id.Sort "Time"))
      ; Term.colon
          (Term.const (Id.mk Id.Term "mtb"))
          (Term.const (Id.mk Id.Sort "Time")) ]
      (* return type *)
      (Term.const (Id.mk Id.Sort "Threevalue"))
      (* body *)
      (Term.apply
         (Term.const (Id.mk Id.Term "mapb3"))
         [ Term.apply
             (Term.const (Id.mk Id.Term ("evalc" ^ id)))
             [Term.const (Id.mk Id.Term "mt"); Term.const (Id.mk Id.Term "mtb")]
         ])
  in
  eventually_eq ("!" ^ string_of_int idx) sf1 ;
  (* "(eventually_op!" ^ (string_of_int idx) ^" (+ mt "^ (string_of_int (int_of_float gamma)) ^") mt t )" *)
  ( Term.apply
      (Term.const (Id.mk Id.Term ("eventually_op!" ^ string_of_int idx)))
      [ f_sum
          (Term.const (Id.mk Id.Term "mt"))
          (Term.const (Id.mk Id.Term freevariable))
      ; Term.const (Id.mk Id.Term "mt") ]
  , []
    (* things are append in until_less; we don't need (snd sf1) *)
  )

let synth_fm_aw_eq gamma sf1 helper =
  synth_fm_not (synth_fm_uless gamma (empty_body f_tvtrue) (synth_fm_not sf1 helper) helper) helper

(*
  synthesis of U=
*)
let synth_fm_ueq gamma sf1 sf2 helper =
  let x, y = synth_fm_aw_eq gamma sf1 helper in
  let x2, y2 = synth_fm_ev_eq gamma sf2 helper in
  (* synth_fm_ueq is equal to synth_fm_aw_eq /\ synth_fm_ev_eq *)
  (f_tvand x x2, y @ y2)

(*
  synthesis of U<=
*)
let synth_fm_ulesseq gamma sf1 sf2 helper =
  let x, y = synth_fm_uless gamma sf1 sf2 helper in
  let x2, y2 = synth_fm_ev_eq gamma sf2 helper in
  (* synth_fm_ulesseq is equal to synth_fm_uless \/ synth_fm_ev_eq *)
  (f_tvor x x2, y @ y2)


let synth_smtlib_header () =
  let common_header () = set_info lst ":smt-lib-version" "2.6" in
  let common_header_cvc4 () =
    if !recursive_unrolling then set_logic lst "QF_AUFDTNIRA"
    else set_logic lst "AUFDTNIRA" ;
    set_info lst ":source" "|https://github.com/anmaped/rmtld3synth|" ;
    set_info lst ":license" "\"https://creativecommons.org/licenses/by/4.0/\"" ;
    (*
		(set-info :category <category>)
		(set-info :status <status>)
		*)
    set_option lst ":produce-models" "true"
  in
  let common_header_z3 () =
    set_option lst ":auto_config" "false" ;
    set_option lst ":model.v2" "true" ;
    set_option lst ":smt.phase_selection" "0" ;
    set_option lst ":smt.restart_strategy" "0" ;
    set_option lst ":smt.restart_factor" "|1.5|" ;
    set_option lst ":smt.arith.random_initial_value" "true" ;
    set_option lst ":smt.case_split" "10" ;
    set_option lst ":smt.delay_units" "true" ;
    set_option lst ":smt.delay_units_threshold" "300" ;
    set_option lst ":smt.qi.eager_threshold" "400"
  in
  common_header () ;
  if isZ3SolverEnabled () then common_header_z3 () else common_header_cvc4 () ;
  ()

let synth_smtlib_common_types () =
  (* (define-sort Proptype () Int) *)
  define_sort lst (Id.mk Id.Sort "Proptype") []
    (Term.const (Id.mk Id.Sort "Int")) ;
  define_sort lst (Id.mk Id.Sort "Time") [] (Term.const (Id.mk Id.Sort "Int")) ;
  define_sort lst (Id.mk Id.Sort "Index") [] (Term.const (Id.mk Id.Sort "Int")) ;
  (* (declare-datatypes ((Duration 0)) (( (dnone) (dsome (val Int) ) ))  ) *)
  define_datatypes lst
    [ ( Id.mk Id.Sort "Duration"
      , []
      , [ (Id.mk Id.Sort "dnone", [])
        ; ( Id.mk Id.Sort "dsome"
          , [ Term.colon
                (Term.const (Id.mk Id.Sort "dval"))
                (Term.const (Id.mk Id.Sort "Int")) ] ) ] ) ] ;
  (* (declare-datatypes (T1 T2) ((Pair (mk-pair (first T1) (second T2))))) --> (declare-datatypes ( (Pair 2) ) ((par (T1 T2) ( (mk-pair (first T1) (second T2)) )))) *)
  define_datatypes lst
    [ ( Id.mk Id.Sort "Pair"
      , [Term.const (Id.mk Id.Attr "T1"); Term.const (Id.mk Id.Attr "T2")]
      , [ ( Id.mk Id.Term "mk-pair"
          , [ Term.colon
                (Term.const (Id.mk Id.Sort "first"))
                (Term.const (Id.mk Id.Sort "T1"))
            ; Term.colon
                (Term.const (Id.mk Id.Sort "second"))
                (Term.const (Id.mk Id.Sort "T2")) ] ) ] ) ] ;
  (* (define-sort Trace () (Array Index Proptype) ) *)
  define_sort lst (Id.mk Id.Sort "Trace") []
    (Term.apply
       (Term.const (Id.mk Id.Sort "Array"))
       [ Term.const (Id.mk Id.Sort "Index")
       ; Term.const (Id.mk Id.Sort "Proptype") ]) ;
  (* (define-sort Trace_ () (Array Index Real) ) *)
  define_sort lst (Id.mk Id.Sort "Trace_") []
    (Term.apply
       (Term.const (Id.mk Id.Sort "Array"))
       [ Term.const (Id.mk Id.Sort "Index")
       ; Term.const (Id.mk Id.Sort "Real") ]) ;
  (* (declare-datatypes () ((Fourvalue (FVTRUE) (FVFALSE) (FVUNKNOWN) (FVSYMBOL) ))) *)
  define_datatypes lst
    [ ( Id.mk Id.Sort "Fourvalue"
      , []
      , [ (Id.mk Id.Term "FVTRUE", [])
        ; (Id.mk Id.Term "FVFALSE", [])
        ; (Id.mk Id.Term "FVUNKNOWN", [])
        ; (Id.mk Id.Term "FVSYMBOL", []) ] ) ] ;
  (* (declare-datatypes () ((Threevalue (TVTRUE) (TVFALSE) (TVUNKNOWN) ))) *)
  define_datatypes lst
    [ ( Id.mk Id.Sort "Threevalue"
      , []
      , [ (Id.mk Id.Term "TVTRUE", [])
        ; (Id.mk Id.Term "TVFALSE", [])
        ; (Id.mk Id.Term "TVUNKNOWN", []) ] ) ] ;
  (*
		(define-fun tvnot ((phi Threevalue)) Threevalue
		(ite (= phi TVTRUE) TVFALSE (ite (= phi TVFALSE) TVTRUE TVUNKNOWN) )
		)
	*)
  define_fun lst
    (* id *)
    (Id.mk Id.Term "tvnot")
    (* arguments *)
    [ Term.colon
        (Term.const (Id.mk Id.Term "phi"))
        (Term.const (Id.mk Id.Sort "Threevalue")) ]
    (* return type *)
    (Term.const (Id.mk Id.Sort "Threevalue"))
    (* body *)
    (ite
       (f_equal
          (Term.const (Id.mk Id.Term "phi"))
          (Term.const (Id.mk Id.Term "TVTRUE")))
       (Term.const (Id.mk Id.Term "TVFALSE"))
       (ite
          (f_equal
             (Term.const (Id.mk Id.Term "phi"))
             (Term.const (Id.mk Id.Term "TVFALSE")))
          (Term.const (Id.mk Id.Term "TVTRUE"))
          (Term.const (Id.mk Id.Term "TVUNKNOWN")))) ;
  (*
		(define-fun tvor ((phi1 Threevalue) (phi2 Threevalue)) Threevalue
			(ite (or (= phi1 TVTRUE) (= phi2 TVTRUE) ) TVTRUE (ite (and (= phi1 TVFALSE) (= phi2 TVFALSE)) TVFALSE TVUNKNOWN ) )
		)
	*)
  define_fun lst
    (* id *)
    (Id.mk Id.Term "tvor")
    (* arguments *)
    [ Term.colon
        (Term.const (Id.mk Id.Term "phi1"))
        (Term.const (Id.mk Id.Sort "Threevalue"))
    ; Term.colon
        (Term.const (Id.mk Id.Term "phi2"))
        (Term.const (Id.mk Id.Sort "Threevalue")) ]
    (* return type *)
    (Term.const (Id.mk Id.Sort "Threevalue"))
    (* body *)
    (ite
       (f_or
          (f_equal
             (Term.const (Id.mk Id.Term "phi1"))
             (Term.const (Id.mk Id.Term "TVTRUE")))
          (f_equal
             (Term.const (Id.mk Id.Term "phi2"))
             (Term.const (Id.mk Id.Term "TVTRUE"))))
       (Term.const (Id.mk Id.Term "TVTRUE"))
       (ite
          (f_and
             (f_equal
                (Term.const (Id.mk Id.Term "phi1"))
                (Term.const (Id.mk Id.Term "TVFALSE")))
             (f_equal
                (Term.const (Id.mk Id.Term "phi2"))
                (Term.const (Id.mk Id.Term "TVFALSE"))))
          (Term.const (Id.mk Id.Term "TVFALSE"))
          (Term.const (Id.mk Id.Term "TVUNKNOWN")))) ;
  (*
		(define-fun tvlessthan ((eta1 Duration) (eta2 Duration)) Threevalue
			   (ite (or (= eta1 dnone) (= eta2 dnone)) TVUNKNOWN (ite (< (dval eta1) (dval eta2) ) TVTRUE TVFALSE) )
		)
	*)
  define_fun lst
    (* id *)
    (Id.mk Id.Term "tvlessthan")
    (* arguments *)
    [ Term.colon
        (Term.const (Id.mk Id.Term "eta1"))
        (Term.const (Id.mk Id.Sort "Duration"))
    ; Term.colon
        (Term.const (Id.mk Id.Term "eta2"))
        (Term.const (Id.mk Id.Sort "Duration")) ]
    (* return type *)
    (Term.const (Id.mk Id.Sort "Threevalue"))
    (* body *)
    (ite
       (f_or
          (f_equal
             (Term.const (Id.mk Id.Term "eta1"))
             (Term.const (Id.mk Id.Term "dnone")))
          (f_equal
             (Term.const (Id.mk Id.Term "eta2"))
             (Term.const (Id.mk Id.Term "dnone"))))
       (Term.const (Id.mk Id.Term "TVUNKNOWN"))
       (ite
          (f_less
             (Term.apply
                (Term.const (Id.mk Id.Term "dval"))
                [Term.const (Id.mk Id.Term "eta1")])
             (Term.apply
                (Term.const (Id.mk Id.Term "dval"))
                [Term.const (Id.mk Id.Term "eta2")]))
          (Term.const (Id.mk Id.Term "TVTRUE"))
          (Term.const (Id.mk Id.Term "TVFALSE")))) ;
  ()

let synth_smtlib_common_macros () =
  (*
		(define-fun mapb4 ( (phi Threevalue) ) Fourvalue
			(ite (= phi TVTRUE ) FVTRUE 
				(ite (= phi TVFALSE ) FVFALSE 
					FVUNKNOWN
			 	)
			 )
		)
	*)
  define_fun lst
    (* id *)
    (Id.mk Id.Term "mapb4")
    (* arguments *)
    [ Term.colon
        (Term.const (Id.mk Id.Term "phi"))
        (Term.const (Id.mk Id.Sort "Threevalue")) ]
    (* return type *)
    (Term.const (Id.mk Id.Sort "Fourvalue"))
    (* body *)
    (ite
       (f_equal
          (Term.const (Id.mk Id.Term "phi"))
          (Term.const (Id.mk Id.Term "TVTRUE")))
       (Term.const (Id.mk Id.Term "FVTRUE"))
       (ite
          (f_equal
             (Term.const (Id.mk Id.Term "phi"))
             (Term.const (Id.mk Id.Term "TVFALSE")))
          (Term.const (Id.mk Id.Term "FVFALSE"))
          (Term.const (Id.mk Id.Term "FVUNKNOWN")))) ;
  (*
		(define-fun mapb3 ( (p (Pair Bool Fourvalue)) ) Threevalue

			(ite (= (second p) FVTRUE) TVTRUE
				(ite (= (second p) FVSYMBOL )
					(ite (first p) TVUNKNOWN TVFALSE) 
					(ite (= (second p) FVFALSE ) TVFALSE 
						 TVUNKNOWN
					)
			 	)
			)
		)
	*)
  define_fun lst
    (* id *)
    (Id.mk Id.Term "mapb3")
    (* arguments *)
    [ Term.colon
        (Term.const (Id.mk Id.Term "p"))
        (Term.apply
           (Term.const (Id.mk Id.Sort "Pair"))
           [ Term.const (Id.mk Id.Sort "Bool")
           ; Term.const (Id.mk Id.Sort "Fourvalue") ]) ]
    (* return type *)
    (Term.const (Id.mk Id.Sort "Threevalue"))
    (* body *)
    (ite
       (f_equal
          (Term.apply
             (Term.const (Id.mk Id.Term "second"))
             [Term.const (Id.mk Id.Term "p")])
          (Term.const (Id.mk Id.Term "FVTRUE")))
       (Term.const (Id.mk Id.Term "TVTRUE"))
       (ite
          (f_equal
             (Term.apply
                (Term.const (Id.mk Id.Term "second"))
                [Term.const (Id.mk Id.Term "p")])
             (Term.const (Id.mk Id.Term "FVSYMBOL")))
          (ite
             (Term.apply
                (Term.const (Id.mk Id.Term "first"))
                [Term.const (Id.mk Id.Term "p")])
             (Term.const (Id.mk Id.Term "TVUNKNOWN"))
             (Term.const (Id.mk Id.Term "TVFALSE")))
          (ite
             (f_equal
                (Term.apply
                   (Term.const (Id.mk Id.Term "second"))
                   [Term.const (Id.mk Id.Term "p")])
                (Term.const (Id.mk Id.Term "FVFALSE")))
             (Term.const (Id.mk Id.Term "TVFALSE"))
             (Term.const (Id.mk Id.Term "TVUNKNOWN"))))) ;
  ()

let synth_smtlib_common_evali () =
  (*
		(define-fun evali ((b1 Threevalue) (b2 Threevalue)) Fourvalue
			(ite (= b2 TVFALSE) (ite (= b1 TVTRUE) FVSYMBOL (mapb4 b1) ) (mapb4 b2) )
		)
	*)
  define_fun lst
    (* id *)
    (Id.mk Id.Term "evali")
    (* arguments *)
    [ Term.colon
        (Term.const (Id.mk Id.Term "b1"))
        (Term.const (Id.mk Id.Sort "Threevalue"))
    ; Term.colon
        (Term.const (Id.mk Id.Term "b2"))
        (Term.const (Id.mk Id.Sort "Threevalue")) ]
    (* return type *)
    (Term.const (Id.mk Id.Sort "Fourvalue"))
    (* body *)
    (ite
       (f_equal
          (Term.const (Id.mk Id.Term "b2"))
          (Term.const (Id.mk Id.Term "TVFALSE")))
       (ite
          (f_equal
             (Term.const (Id.mk Id.Term "b1"))
             (Term.const (Id.mk Id.Term "TVTRUE")))
          (Term.const (Id.mk Id.Term "FVSYMBOL"))
          (Term.apply
             (Term.const (Id.mk Id.Term "mapb4"))
             [Term.const (Id.mk Id.Term "b1")]))
       (Term.apply
          (Term.const (Id.mk Id.Term "mapb4"))
          [Term.const (Id.mk Id.Term "b2")])) ;
  ()

let synth_smtlib_common_trace () =
  (*
		(declare-const trc Trace )
		(declare-const trc_size Time)
	*)
  declare_const lst (Id.mk Id.Term "trc") (Term.const (Id.mk Id.Sort "Trace")) ;
  if not !assume_unary_sequence then declare_const lst (Id.mk Id.Term "trc_time") (Term.const (Id.mk Id.Sort "Trace_")) ;
  declare_const lst (Id.mk Id.Term "trc_size")
    (Term.const (Id.mk Id.Sort "Time")) ;

  if not !assume_unary_sequence then (
  declare_fun lst
  (* id *)
  (Id.mk Id.Term "mapt")
  (* arguments *)
  [
    Term.const (Id.mk Id.Sort "Index") ]
  (* return type *)
  (Term.const (Id.mk Id.Sort "Real")) ;

  if !recursive_unrolling then (
      (* unrooling the recursion just in case (speedup) *)
      let enumeration = List.of_enum (0 -- !recursive_unrolling_depth) in
      (*let lst_all_comb = cartesian enumeration enumeration in*)
      List.fold_left
          (fun _ i -> if 0 < i then
         assert_ lst ( ( f_equal (Term.apply (f_const_term "mapt") [f_const_term (string_of_int i)]) (f_sum (Term.apply (f_const_term "mapt") [f_minus (f_const_term (string_of_int i)) (f_const_term "1") ]) (Term.apply (Term.const (Id.mk Id.Term "select")) [ Term.const (Id.mk Id.Term "trc_time"); Term.const (Id.mk Id.Term (string_of_int i)) ]) ) ) )
          else
          assert_ lst ( ( f_equal (Term.apply (f_const_term "mapt") [f_const_term (string_of_int i)]) (Term.apply (Term.const (Id.mk Id.Term "select")) [ Term.const (Id.mk Id.Term "trc_time"); Term.const (Id.mk Id.Term (string_of_int i)) ]) ) )
           ) () enumeration ;

assert_ lst ( f_equal (Term.apply (f_const_term "mapt") [f_const_term "(- 1)"]) (f_const_term "0") ) ;
         (* (assert (= (select trc_time 0) 0 ))  *)
  (*assert_ lst (f_equal (f_const_term "0") (Term.apply (f_const_term "select") [ f_const_term "trc_time"; f_const_term "0" ]) ) ;*)
 )
else
(
  assert_ lst (
    Term.forall
      [ Term.colon (f_const_term "i") (f_const_sort "Index") ]
      (
(ite
(f_less (f_const_term "0") (f_const_term "i") )
( f_equal (Term.apply (f_const_term "mapt") [f_const_term "i"]) (f_sum (Term.apply (f_const_term "mapt") [f_minus (f_const_term "i") (f_const_term "1") ]) (Term.apply (Term.const (Id.mk Id.Term "select")) [ Term.const (Id.mk Id.Term "trc_time"); Term.const (Id.mk Id.Term "i") ]) ) )
( f_equal (Term.apply (f_const_term "mapt") [f_const_term "i"]) (Term.apply (Term.const (Id.mk Id.Term "select")) [ Term.const (Id.mk Id.Term "trc_time"); Term.const (Id.mk Id.Term "i") ]) )
)

      )
    ) ;

 
) ;
) ;
  ()

let synth_smtlib_common_prop () =
  (*
		(define-fun ev_prop ( (mk Trace) (mt Time) (p Proptype) ) Threevalue
			(ite (>= trc_size mt) (ite (= (select mk mt) p) TVTRUE TVFALSE ) TVUNKNOWN )
		)
	*)
  define_fun lst
    (* id *)
    (Id.mk Id.Term "ev_prop")
    (* arguments *)
    [ Term.colon
        (Term.const (Id.mk Id.Term "mk"))
        (Term.const (Id.mk Id.Sort "Trace"))
    ; Term.colon
        (Term.const (Id.mk Id.Term "mt"))
        (Term.const (Id.mk Id.Sort "Time"))
    ; Term.colon
        (Term.const (Id.mk Id.Term "p"))
        (Term.const (Id.mk Id.Sort "Proptype")) ]
    (* return type *)
    (Term.const (Id.mk Id.Sort "Threevalue"))
    (* body *)
    (ite
       (f_geq
          (Term.const (Id.mk Id.Term "trc_size"))
          (Term.const (Id.mk Id.Term "mt")))
       (ite
          (f_equal
             (Term.apply
                (Term.const (Id.mk Id.Term "select"))
                [ Term.const (Id.mk Id.Term "mk")
                ; Term.const (Id.mk Id.Term "mt") ])
             (Term.const (Id.mk Id.Term "p")))
          (Term.const (Id.mk Id.Term "TVTRUE"))
          (Term.const (Id.mk Id.Term "TVFALSE")))
       (Term.const (Id.mk Id.Term "TVUNKNOWN"))) ;
  ()

let synth_smtlib synth_fun formula helper =
  synth_smtlib_header () ;
  synth_smtlib_common_types () ;
  synth_smtlib_common_macros () ;
  synth_smtlib_common_evali () ;
  synth_smtlib_common_trace () ;
  synth_smtlib_common_prop () ;
  let tm, stm = synth_fun formula helper in
  add_l stm ;
  (* (define-fun allcheck  ((mk Trace) (mt Time)) Bool (= "^ tm ^" TVTRUE) ) *)
  define_fun lst
    (* id *)
    (Id.mk Id.Term "allcheck")
    (* arguments *)
    [ Term.colon
        (Term.const (Id.mk Id.Term "mk"))
        (Term.const (Id.mk Id.Sort "Trace"))
    ; Term.colon
        (Term.const (Id.mk Id.Term "mt"))
        (Term.const (Id.mk Id.Sort "Time")) ]
    (* return type *)
    (Term.const (Id.mk Id.Sort "Bool"))
    (* body *)
    (f_equal tm (Term.const (Id.mk Id.Term "TVTRUE"))) ;
  (*  if isZ3SolverEnabled () then (assert (forall ((t Time)) (>= (select trc t) 0)  )) *)
  if isZ3SolverEnabled () then
    assert_ lst
      (Term.forall
         [ Term.colon
             (Term.const (Id.mk Id.Term "t"))
             (Term.const (Id.mk Id.Sort "Time")) ]
         (f_geq
            (Term.apply
               (Term.const (Id.mk Id.Term "select"))
               [ Term.const (Id.mk Id.Term "trc")
               ; Term.const (Id.mk Id.Term "t") ])
            (Term.const (Id.mk Id.Term "0")))) ;
    if not !assume_unary_sequence then
    assert_ lst
      (Term.forall
         [ Term.colon
             (Term.const (Id.mk Id.Term "t"))
             (Term.const (Id.mk Id.Sort "Time")) ]
         (f_less
            (Term.const (Id.mk Id.Term "0"))
            (Term.apply
               (Term.const (Id.mk Id.Term "select"))
               [ Term.const (Id.mk Id.Term "trc_time")
               ; Term.const (Id.mk Id.Term "t") ])
            )) ;
  (* (assert (allcheck trc 0) ) *)
  add
    (Statement.assert_
       (Term.apply
          (Term.const (Id.mk Id.Term "allcheck"))
          [Term.const (Id.mk Id.Term "trc"); f_const_term "0"])) ;
  if not (isZ3SolverEnabled ()) then
  begin
    check_sat lst ;
    get_model lst ;
    get_info lst ":all-statistics" ;
  end ;
  (* if isZ3SolverEnabled () then "(check-sat-using (then qe smt))" ; this entry is available only in Z3 *)

  (* pretty print smtlib statements to string *)
  List.iter
    (fun a ->
      verb_m 2 (fun _ ->
          Statement.print Format.std_formatter a ;
          Format.pp_print_newline Format.std_formatter (); ) ;
      (* print smtlib *)
      Dolmen_export.Smtlib.print Format.str_formatter a )
    (List.rev !lst) ;
  Format.flush_str_formatter ()
