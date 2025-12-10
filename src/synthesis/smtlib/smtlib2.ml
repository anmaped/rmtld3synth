open Sexplib
open Sexplib.Conv
open Dolmen.Std
open Dolmen_loop
open Rmtld3
open Helper

type body = Term.t

module State = Dolmen_loop.State
module Typer = Dolmen_loop.Typer.Typer (State)
module Typer_Pipe =
  Dolmen_loop.Typer.Make (Dolmen.Std.Expr) (Dolmen.Std.Expr.Print) (State)
    (Typer)
module Export =
  Dolmen_loop.Export.Make (Dolmen.Std.Expr) (Dolmen.Std.Term.View.Sexpr)
    (Dolmen_std.Expr.View.TFF)
    (State)
    (Typer_Pipe)

let recursive_unrolling helper = get_setting_bool "rec_unrolling" helper

let recursive_unrolling_depth helper =
  if is_setting "rec_unrolling_depth" helper then
    get_setting_int "rec_unrolling_depth" helper
  else failwith "No 'rec_unrolling_depth' setting found!"

let assume_unary_sequence helper =
  get_setting_bool "assume_unary_sequence" helper

let isZ3SolverEnabled helper =
  if is_setting "solver" helper then
    let s = get_setting_string "solver" helper in
    s = "z3"
  else false

let isCvc4SolverEnabled helper =
  if is_setting "solver" helper then
    let s = get_setting_string "solver" helper in
    s = "cvc4"
  else false

let init_set_variables helper =
  (* check if hash is already there *)
  if is_setting "variables" helper then (
    (* do reset hash table *)
    let h = get_setting_hash "variables" helper in
    Hashtbl.reset h ;
    Hashtbl.add h (S "free_variables_set") (S (set_to_string SS.empty)) ;
    Hashtbl.add h (S "lst") (S (list_to_string [])) )
  else
    let h = Hashtbl.create 5 in
    Hashtbl.add h (S "free_variables_set") (S (set_to_string SS.empty)) ;
    Hashtbl.add h (S "lst") (S (list_to_string [])) ;
    set_setting "variables" (Hash h) helper

let set_variable_lst lst helper =
  let lst_str = list_to_string lst in
  set_variable "lst" (S lst_str) helper

let get_variable_lst helper =
  let s = get_variable "lst" helper in
  string_to_list s

let add a helper =
  let lst = get_variable_lst helper in
  set_variable_lst (a :: lst) helper

let add_l l helper =
  let lst = get_variable_lst helper in
  set_variable_lst (l @ lst) helper

let set_option_bool a b helper =
  let lst = get_variable_lst helper in
  set_variable_lst
    ( Statement.set_option
        (Term.apply (Term.const (Id.mk Id.attr a)) [Term.quoted b])
    :: lst )
    helper

let set_option_real a b helper =
  let lst = get_variable_lst helper in
  set_variable_lst
    ( Statement.set_option
        (Term.apply (Term.const (Id.mk Id.attr a)) [Term.real b])
    :: lst )
    helper

let set_option_int a b helper =
  let lst = get_variable_lst helper in
  set_variable_lst
    ( Statement.set_option
        (Term.apply (Term.const (Id.mk Id.attr a)) [Term.int b])
    :: lst )
    helper

let set_info a b helper =
  let lst = get_variable_lst helper in
  set_variable_lst
    ( Statement.set_info (Term.apply (Term.const (Id.mk Id.attr a)) [b])
    :: lst )
    helper

let set_info_str a b helper =
  let lst = get_variable_lst helper in
  set_variable_lst
    ( Statement.set_info
        (Term.apply (Term.const (Id.mk Id.attr a)) [Term.str b])
    :: lst )
    helper

let set_info_quoted a b helper =
  let lst = get_variable_lst helper in
  set_variable_lst
    ( Statement.set_info
        (Term.apply (Term.const (Id.mk Id.attr a)) [Term.quoted b])
    :: lst )
    helper

let set_logic a helper =
  let lst = get_variable_lst helper in
  set_variable_lst (Statement.set_logic a :: lst) helper

let define_sort id args body helper =
  let lst = get_variable_lst helper in
  set_variable_lst (Statement.type_def id args body :: lst) helper

let define_fun id args ty_rec body helper =
  let lst = get_variable_lst helper in
  set_variable_lst (Statement.fun_def id [] args ty_rec body :: lst) helper

let define_datatypes a helper =
  let lst = get_variable_lst helper in
  set_variable_lst (Statement.datatypes a :: lst) helper

let check_sat helper =
  let lst = get_variable_lst helper in
  set_variable_lst (Statement.check_sat [] :: lst) helper

let get_model helper =
  let lst = get_variable_lst helper in
  set_variable_lst (Statement.get_model () :: lst) helper

let get_info a helper =
  let lst = get_variable_lst helper in
  set_variable_lst
    (Statement.get_info (Term.const (Id.mk Id.attr a)) :: lst)
    helper

let declare_const a typ helper =
  let lst = get_variable_lst helper in
  set_variable_lst (Statement.decl a typ :: lst) helper

let declare_fun a l typ helper =
  let lst = get_variable_lst helper in
  set_variable_lst (Statement.fun_decl a [] l typ :: lst) helper

let assert_ a helper =
  let lst = get_variable_lst helper in
  set_variable_lst (Statement.assert_ a :: lst) helper

let f_const_term a = Term.const (Id.mk Id.term a)

let f_const_sort a = Term.const (Id.mk Id.sort a)

let f_equal a b = Term.apply (f_const_term "=") [a; b]

let ite a b c = Term.apply (f_const_term "ite") [a; b; c]

let f_true = f_const_term "true"

let f_tvtrue = f_const_term "TVTRUE"

let f_fvtrue = f_const_term "FVTRUE"

let f_not a = Term.apply (f_const_term "not") [a]

let f_tvnot a = Term.apply (f_const_term "tvnot") [a]

let f_or a b = Term.apply (f_const_term "or") [a; b]

let f_tvor a b = Term.apply (f_const_term "tvor") [a; b]

let f_and a b = Term.apply (f_const_term "and") [a; b]

let f_tvand a b = f_tvnot (f_tvor (f_tvnot a) (f_tvnot b))

let f_implies a b = Term.apply (f_const_term "=>") [a; b]

let f_less a b = Term.apply (f_const_term "<") [a; b]

let f_leq a b = Term.apply (f_const_term "<=") [a; b]

let f_geq a b = Term.apply (f_const_term ">=") [a; b]

let f_sum a b = Term.apply (f_const_term "+") [a; b]

let f_minus a b = Term.apply (f_const_term "-") [a; b]

let f_times a b = Term.apply (f_const_term "*") [a; b]

let cartesian l l' =
  List.concat (List.map (fun e -> List.map (fun e' -> (e, e')) l') l)

(* Synthesis *)

(* parameterized synthesis functions for until operators *)
let evalfold_param id helper =
  let evalfold id =
    (* (declare-fun evalfold" ^ id ^ " (Time Time) Fourvalue ) *)
    declare_fun
      (Id.mk Id.term ("evalfold" ^ id))
      [f_const_sort "Time"; f_const_sort "Time"]
      (f_const_sort "Fourvalue")
      helper ;
    if recursive_unrolling helper then (
      (* unrooling the recursion just in case (speedup) *)
      let enumeration = of_enum 0 (recursive_unrolling_depth helper) in
      let lst_all_comb = cartesian enumeration enumeration in
      (* (assert (forall ((x Time) (i Time)) (implies (not (and (and (<= 0 x)
         (<= x max_depth) ) (and (<= 0 i) (<= i max_depth) ) ) ) (=
         (evalfold!id x i) FVTRUE ) ) ) ) *)
      (* this assert closes the unroll search space with upper and lower
         bound for freevariables x and i *)
      assert_
        (Term.forall
           [ Term.colon (f_const_term "x") (f_const_sort "Time")
           ; Term.colon (f_const_term "i") (f_const_sort "Time") ]
           (f_implies
              (f_not
                 (f_and
                    (f_and
                       (f_leq (Term.int "0") (f_const_term "x"))
                       (f_leq (f_const_term "x")
                          (Term.int
                             (string_of_int
                                (recursive_unrolling_depth helper) ) ) ) )
                    (f_and
                       (f_leq (Term.int "0") (f_const_term "i"))
                       (f_leq (f_const_term "i")
                          (Term.int
                             (string_of_int
                                (recursive_unrolling_depth helper) ) ) ) ) ) )
              (f_equal
                 (Term.apply
                    (f_const_term ("evalfold" ^ id))
                    [f_const_term "x"; f_const_term "i"] )
                 f_fvtrue ) ) )
        helper ;
      List.fold_left
        (fun a (x, i) ->
          if x > i then
            (* (assert (= (evalb" ^ id ^ " trc "^ string_of_int x ^" "^
               string_of_int i ^" (evalfold" ^ id ^ " (- "^ string_of_int x
               ^" 1) "^ string_of_int i ^" )) (evalfold" ^ id ^ " "^
               string_of_int x ^" "^ string_of_int i ^" ) ) ) *)
            assert_
              (f_equal
                 (Term.apply
                    (f_const_term ("evalb" ^ id))
                    [ f_const_term "trc"
                    ; Term.int (string_of_int x)
                    ; Term.int (string_of_int i)
                    ; Term.apply
                        (f_const_term ("evalfold" ^ id))
                        [ f_minus (Term.int (string_of_int x)) (Term.int "1")
                        ; Term.int (string_of_int i) ] ] )
                 (Term.apply
                    (f_const_term ("evalfold" ^ id))
                    [Term.int (string_of_int x); Term.int (string_of_int i)] ) )
              helper
          else
            (* (assert (= (evalb" ^ id ^ " trc "^ string_of_int x ^" "^
               string_of_int i ^" FVSYMBOL) (evalfold" ^ id ^ " "^
               string_of_int x ^" "^ string_of_int i ^")) ) *)
            assert_
              (f_equal
                 (Term.apply
                    (f_const_term ("evalb" ^ id))
                    [ f_const_term "trc"
                    ; Term.int (string_of_int x)
                    ; Term.int (string_of_int i)
                    ; f_const_term "FVSYMBOL" ] )
                 (Term.apply
                    (f_const_term ("evalfold" ^ id))
                    [Term.int (string_of_int x); Term.int (string_of_int i)] ) )
              helper )
        () lst_all_comb ;
      () )
    else (
      (* (assert (forall ((x Time) (i Time)) (ite (> x i) (= (evalb" ^ id ^ "
         trc x i (evalfold" ^ id ^ " (- x 1) i )) (evalfold" ^ id ^ " x i ) )
         (= (evalb" ^ id ^ " trc x i FVSYMBOL) (evalfold" ^ id ^ " x i)) )
         )) *)
      assert_
        (Term.forall
           [ Term.colon (f_const_term "x") (f_const_sort "Time")
           ; Term.colon (f_const_term "i") (f_const_sort "Time") ]
           (ite
              (f_less (f_const_term "i") (f_const_term "x"))
              (f_equal
                 (Term.apply
                    (f_const_term ("evalb" ^ id))
                    [ f_const_term "trc"
                    ; f_const_term "x"
                    ; f_const_term "i"
                    ; Term.apply
                        (f_const_term ("evalfold" ^ id))
                        [ f_minus (f_const_term "x") (Term.int "1")
                        ; f_const_term "i" ] ] )
                 (Term.apply
                    (f_const_term ("evalfold" ^ id))
                    [f_const_term "x"; f_const_term "i"] ) )
              (f_equal
                 (Term.apply
                    (f_const_term ("evalb" ^ id))
                    [ f_const_term "trc"
                    ; f_const_term "x"
                    ; f_const_term "i"
                    ; f_const_term "FVSYMBOL" ] )
                 (Term.apply
                    (f_const_term ("evalfold" ^ id))
                    [f_const_term "x"; f_const_term "i"] ) ) ) )
        helper ;
      () )
  in
  evalfold id

let dummy_tuple = (f_const_term "dummy", [])

let synth_tm_constant value helper =
  Term.apply (f_const_term "dsome")
    [Term.int (string_of_int (int_of_float value))]

let synth_tm_variable name helper =
  let free_variables_set =
    string_to_set (get_variable "free_variables_set" helper)
  in
  if not (SS.exists (fun s -> s = name) free_variables_set) then (
    let free_variables_set = SS.add name free_variables_set in
    set_variable "free_variables_set"
      (S (set_to_string free_variables_set))
      helper ;
    declare_const (Id.mk Id.term name)
      ( if assume_unary_sequence helper then f_const_sort "Int"
        else f_const_sort "Real" )
      helper ) ;
  Term.apply (f_const_term "dsome") [f_const_term name]

let synth_tm_duration tm_call fm_call helper =
  let idx = get_duration_counter helper in
  let freevariable = "v_dt!" ^ string_of_int idx in
  declare_const (Id.mk Id.term freevariable) (f_const_sort "Duration") helper ;
  if assume_unary_sequence helper then
    assert_ (f_equal (f_const_term freevariable) tm_call) helper
  else if recursive_unrolling helper then
    ( assert_
        (f_and
           (f_less
              (Term.apply (f_const_term "dval") [f_const_term freevariable])
              (Term.int (string_of_int (recursive_unrolling_depth helper))) )
           (f_less (Term.int "0")
              (Term.apply (f_const_term "dval") [f_const_term freevariable]) ) )
        helper ;
      (* (assert (forall ((i Index)) (<= 0 (select trc_time i) ) ) ) *)
      assert_
        (Term.forall
           [Term.colon (f_const_term "i") (f_const_sort "Index")]
           ((f_and
               (f_less
                  (Term.apply (f_const_term "select")
                     [f_const_term "trc_time"; f_const_term "i"] )
                  (Term.int (string_of_int 100)) ) )
              (f_less (Term.int "0")
                 (Term.apply (f_const_term "select")
                    [f_const_term "trc_time"; f_const_term "i"] ) ) ) ) )
      helper ;
  let duration id fm =
    let indicator id =
      (* (define-fun indicator"^ id ^" ((mk Trace) (mt Time)) Int (ite (= "^
         formula ^" TVTRUE) 1 0) ) *)
      define_fun
        (* id *)
        (Id.mk Id.term ("indicator" ^ id))
        (* arguments *)
        [ Term.colon (f_const_term "mk") (f_const_sort "Trace")
        ; Term.colon (f_const_term "mt") (f_const_sort "Time") ]
        (* return type *)
        (f_const_sort "Int")
        (* body *)
        (ite
           (f_equal fm (f_const_term "TVTRUE"))
           (Term.int "1") (Term.int "0") )
        helper
    in
    let evaleta id =
      (* (declare-fun evaleta"^ id ^" ((Time) (Time)) Duration) *)
      declare_fun
        (Id.mk Id.term ("evaleta" ^ id))
        [f_const_sort "Time"; f_const_sort "Time"]
        (f_const_sort "Duration") helper ;
      if recursive_unrolling helper then (
        (* unrooling the recursion just in case (speedup) *)
        let enumeration = of_enum 0 (recursive_unrolling_depth helper) in
        let lst_all_comb = cartesian enumeration enumeration in
        (* (assert (forall ((x Time) (i Time)) (implies (not (and (and (<= 0
           x) (<= x max_depth) ) (and (<= 0 i) (<= i max_depth) ) ) ) (=
           evaleta!id x i) dnone ) ) ) ) *)
        (* this assert closes the unroll search space with upper and lower
           bound for freevariables x and i *)
        assert_
          (Term.forall
             [ Term.colon (f_const_term "x") (f_const_sort "Time")
             ; Term.colon (f_const_term "i") (f_const_sort "Time") ]
             (f_implies
                (f_not
                   (f_and
                      (f_and
                         (f_leq (Term.int "0") (f_const_term "x"))
                         (f_leq (f_const_term "x")
                            (Term.int
                               (string_of_int
                                  (recursive_unrolling_depth helper) ) ) ) )
                      (f_and
                         (f_leq (Term.int "0") (f_const_term "i"))
                         (f_leq (f_const_term "i")
                            (Term.int
                               (string_of_int
                                  (recursive_unrolling_depth helper) ) ) ) ) ) )
                (f_equal
                   (Term.apply
                      (f_const_term ("evaleta" ^ id))
                      [f_const_term "x"; f_const_term "i"] )
                   (f_const_term "dnone") ) ) )
          helper ;
        List.fold_left
          (fun a (x, i) ->
            if x > i then
              (* (assert (= (evaleta"^ id ^" "^ string_of_int x ^" "^
                 string_of_int i ^") (+ (evaleta"^ id ^" (- "^ string_of_int
                 x ^" 1) "^ string_of_int i ^") (indicator"^ id ^" trc "^
                 string_of_int x ^") )) ) *)
              assert_
                (f_equal
                   (Term.apply (f_const_term "dval")
                      [ Term.apply
                          (f_const_term ("evaleta" ^ id))
                          [ Term.int (string_of_int x)
                          ; Term.int (string_of_int i) ] ] )
                   (f_sum
                      (Term.apply (f_const_term "dval")
                         [ Term.apply
                             (f_const_term ("evaleta" ^ id))
                             [ f_minus
                                 (Term.int (string_of_int x))
                                 (Term.int "1")
                             ; Term.int (string_of_int i) ] ] )
                      (f_times
                         (Term.apply
                            (f_const_term ("indicator" ^ id))
                            [f_const_term "trc"; Term.int (string_of_int x)] )
                         ( if assume_unary_sequence helper then Term.int "1"
                           else
                             Term.apply (f_const_term "select")
                               [ f_const_term "trc_time"
                               ; Term.int (string_of_int x) ] ) ) ) )
                helper
            else
              (* (assert (= (evaleta"^ id ^" "^ string_of_int x ^" "^
                 string_of_int i ^") (indicator"^ id ^" trc "^ string_of_int
                 x ^") ) ) *)
              assert_
                (f_equal
                   (Term.apply (f_const_term "dval")
                      [ Term.apply
                          (f_const_term ("evaleta" ^ id))
                          [ Term.int (string_of_int x)
                          ; Term.int (string_of_int i) ] ] )
                   (f_times
                      (Term.apply
                         (f_const_term ("indicator" ^ id))
                         [f_const_term "trc"; Term.int (string_of_int x)] )
                      ( if assume_unary_sequence helper then Term.int "1"
                        else
                          Term.apply (f_const_term "select")
                            [ f_const_term "trc_time"
                            ; Term.int (string_of_int x) ] ) ) )
                helper )
          () lst_all_comb ;
        () )
      else (
        (* (assert (forall ((x Time) (i Time)) (=> (and (>= x 0) (<= x (+ "^
           (string_of_int t) ^" "^ dt ^") )) (ite (and (and (< x (+ "^
           (string_of_int t) ^" "^ dt ^") ) (>= i 0)) (> x i)) (= (evaleta"^
           id ^" x i) (+ (evaleta"^ id ^" (- x 1) i) (indicator"^ id ^" trc
           x) )) (= (evaleta"^ id ^" x i) (indicator"^ id ^" trc x) ) )))
           ) *)
        assert_
          (Term.forall
             [ Term.colon (f_const_term "x") (f_const_sort "Time")
             ; Term.colon (f_const_term "i") (f_const_sort "Time") ]
             (f_implies
                (*(and (>= x 0) (<= x (+ "^ (string_of_int t) ^" "^ dt ^")
                  ))*)
                (f_geq (f_const_term "x") (Term.int "0"))
                (* ignores upper bound *)
                (ite
                   (* (and (and (< x (+ "^ (string_of_int t) ^" "^ dt ^") ) (>= i 0)) (> x i)) *)
                   (* ignores upper bound *)
                   (f_and
                      (f_geq (f_const_term "i") (Term.int "0"))
                      (f_less (f_const_term "i") (f_const_term "x")) )
                   (* (= (evaleta"^ id ^" x i) (+ (evaleta"^ id ^" (- x 1) i)
                      (indicator"^ id ^" trc x) )) *)
                   (f_equal
                      (f_sum
                         (Term.apply (f_const_term "dval")
                            [ Term.apply
                                (f_const_term ("evaleta" ^ id))
                                [ f_minus (f_const_term "x") (Term.int "1")
                                ; f_const_term "i" ] ] )
                         (Term.apply
                            (f_const_term ("indicator" ^ id))
                            [f_const_term "trc"; f_const_term "x"] ) )
                      (Term.apply (f_const_term "dval")
                         [ Term.apply
                             (f_const_term ("evaleta" ^ id))
                             [f_const_term "x"; f_const_term "i"] ] ) )
                   (* (= (evaleta"^ id ^" x i) (indicator"^ id ^" trc x) ) *)
                   (f_equal
                      (Term.apply (f_const_term "dval")
                         [ Term.apply
                             (f_const_term ("evaleta" ^ id))
                             [f_const_term "x"; f_const_term "i"] ] )
                      (Term.apply
                         (f_const_term ("indicator" ^ id))
                         [f_const_term "trc"; f_const_term "x"] ) ) ) ) )
          helper ;
        () )
    in
    indicator id ;
    evaleta id ;
    (* (define-fun duration_op"^ id ^" ((mt Time) (mtb Time)) Duration (and
       (>= trc_size mt) (evaleta"^ id ^" (- mt 1) mtb) ) ) *)
    define_fun
      (* id *)
      (Id.mk Id.term ("duration_op" ^ id))
      (* arguments *)
      [ Term.colon (f_const_term "mt") (f_const_sort "Time")
      ; Term.colon (f_const_term "mtb") (f_const_sort "Time") ]
      (* return type *)
      (f_const_sort "Duration")
      (* body *)
      (ite
         (f_and
            (f_geq (f_const_term "trc_size") (f_const_term "mt"))
            ( if (* one assumption *)
                 not (assume_unary_sequence helper) then
                f_less
                  (Term.apply (f_const_term "dval") [tm_call])
                  (f_minus
                     (Term.apply (f_const_term "mapt") [f_const_term "mt"])
                     (Term.apply (f_const_term "mapt") [f_const_term "mtb"]) )
              else f_true ) )
         (Term.apply
            (f_const_term ("evaleta" ^ id))
            [f_minus (f_const_term "mt") (Term.int "1"); f_const_term "mtb"] )
         (f_const_term "dnone") )
      helper ;
    (* this symbol is not correct; it should be the bottom_duration *)
    (* (duration_op"^ id ^" (+ mt "^ dt ^") mt) *)
    Term.apply
      (f_const_term ("duration_op" ^ id))
      [ f_sum (f_const_term "mt")
          (Term.apply (f_const_term "dval") [f_const_term freevariable])
      ; f_const_term "mt" ]
  in
  duration ("!" ^ string_of_int idx) fm_call

let synth_tm_plus cmptr1 cmptr2 helper =
  Term.apply (f_const_term "dsome")
    [ f_sum
        (Term.apply (f_const_term "dval") [cmptr1])
        (Term.apply (f_const_term "dval") [cmptr2]) ]

let synth_tm_times cmptr1 cmptr2 helper =
  Term.apply (f_const_term "dsome")
    [ f_times
        (Term.apply (f_const_term "dval") [cmptr1])
        (Term.apply (f_const_term "dval") [cmptr2]) ]

let synth_fm_true helper = f_const_term "TVTRUE"

let synth_fm_p p helper =
  Term.apply (f_const_term "ev_prop")
    [f_const_term "mk"; f_const_term "mt"; Term.int (string_of_int p)]

let synth_fm_not cmpfm helper = Term.apply (f_const_term "tvnot") [cmpfm]

let synth_fm_or cmpfm1 cmpfm2 helper =
  Term.apply (f_const_term "tvor") [cmpfm1; cmpfm2]

let synth_fm_less cmptr1 cmptr2 helper =
  Term.apply (f_const_term "tvlessthan") [cmptr1; cmptr2]

(* synthesis of U< *)
let synth_fm_uless gamma sf1 sf2 helper =
  let idx = get_until_counter helper in
  let freevariable = "v_gamma!" ^ string_of_int idx in
  declare_const (Id.mk Id.term freevariable) (f_const_sort "Index") helper ;
  assert_
    (f_equal
       (f_const_term freevariable)
       (Term.int (string_of_int (int_of_float gamma))) )
    helper ;
  let until_less id comp1 comp2 =
    let evalb id =
      (* (define-fun evalb" ^ id ^ " ( (mk Trace) (mt Time) (mtb Time) (v
         Fourvalue) ) Fourvalue (ite (= v FVSYMBOL) (evali "^ comp1 ^" "^
         comp2 ^" ) v ) ) *)
      define_fun
        (* id *)
        (Id.mk Id.term ("evalb" ^ id))
        (* arguments *)
        [ Term.colon (f_const_term "mk") (f_const_sort "Trace")
        ; Term.colon (f_const_term "mt") (f_const_sort "Time")
        ; Term.colon (f_const_term "mtb") (f_const_sort "Time")
        ; Term.colon (f_const_term "v") (f_const_sort "Fourvalue") ]
        (* return type *)
        (f_const_sort "Fourvalue")
        (* body *)
        (ite
           (f_equal (f_const_term "v") (f_const_term "FVSYMBOL"))
           (Term.apply (f_const_term "evali") [comp1; comp2])
           (f_const_term "v") )
        helper
    in
    let evalfold id = evalfold_param id helper in
    let evalc id =
      (* (define-fun evalc" ^ id ^ " ((mt Time) (mtb Time) ) (Pair Bool
         Fourvalue) (mk-pair (<= trc_size (+ " ^ (string_of_int gamma) ^ "
         mtb ) ) (evalfold" ^ id ^ " (- mt 1) mtb )) ) *)
      define_fun
        (* id *)
        (Id.mk Id.term ("evalc" ^ id))
        (* arguments *)
        [ Term.colon (f_const_term "mt") (f_const_sort "Time")
        ; Term.colon (f_const_term "mtb") (f_const_sort "Time") ]
        (* return type *)
        (Term.apply (f_const_sort "Pair")
           [f_const_sort "Bool"; f_const_sort "Fourvalue"] )
        (* body *)
        (Term.apply (f_const_term "mk-pair")
           [ f_geq
               (f_sum (f_const_term freevariable) (f_const_term "mtb"))
               (f_const_term "trc_size")
           ; Term.apply
               (f_const_term ("evalfold" ^ id))
               [ f_minus (f_const_term "mt") (Term.int "1")
               ; f_const_term "mtb" ] ] )
        helper
    in
    evalb id ;
    evalfold id ;
    evalc id ;
    (* (define-fun until_less_op" ^ id ^ " ((mt Time) (mtb Time) ) Threevalue
       (mapb3 (evalc" ^ id ^ " mt mtb )) ) *)
    define_fun
      (* id *)
      (Id.mk Id.term ("until_less_op" ^ id))
      (* arguments *)
      [ Term.colon (f_const_term "mt") (f_const_sort "Time")
      ; Term.colon (f_const_term "mtb") (f_const_sort "Time") ]
      (* return type *)
      (f_const_sort "Threevalue")
      (* body *)
      (Term.apply (f_const_term "mapb3")
         [ Term.apply
             (f_const_term ("evalc" ^ id))
             [f_const_term "mt"; f_const_term "mtb"] ] )
      helper
  in
  until_less ("!" ^ string_of_int idx) sf1 sf2 ;
  (* "(until_less_op!" ^ (string_of_int idx) ^" (+ mt "^ (string_of_int
     (int_of_float gamma)) ^") mt t )" *)
  Term.apply
    (f_const_term ("until_less_op!" ^ string_of_int idx))
    [f_sum (f_const_term "mt") (f_const_term freevariable); f_const_term "mt"]

let synth_fm_ev_eq gamma sf1 helper =
  let idx = get_until_counter helper in
  let freevariable = "v_gamma!" ^ string_of_int idx in
  declare_const (Id.mk Id.term freevariable) (f_const_sort "Index") helper ;
  assert_
    (f_equal
       (f_const_term freevariable)
       (Term.int (string_of_int (int_of_float gamma))) )
    helper ;
  let eventually_eq id comp1 =
    let evalb id =
      (* (define-fun evalb" ^ id ^ " ( (mk Trace) (mt Time) (mtb Time) (v
         Fourvalue) ) Fourvalue (ite (= v FVSYMBOL) (evali "^ comp1 ^" "^
         comp2 ^" ) v ) ) *)
      define_fun
        (* id *)
        (Id.mk Id.term ("evalb" ^ id))
        (* arguments *)
        [ Term.colon (f_const_term "mk") (f_const_sort "Trace")
        ; Term.colon (f_const_term "mt") (f_const_sort "Time")
        ; Term.colon (f_const_term "mtb") (f_const_sort "Time")
        ; Term.colon (f_const_term "v") (f_const_sort "Fourvalue") ]
        (* return type *)
        (f_const_sort "Fourvalue")
        (* body *)
        (ite
           (f_equal (f_const_term "v") (f_const_term "FVSYMBOL"))
           (Term.apply (f_const_term "evali") [f_tvtrue; comp1])
           (f_const_term "v") )
        helper
    in
    let evalc id =
      (* (define-fun evalc" ^ id ^ " ((mt Time) (mtb Time) ) (Pair Bool
         Fourvalue) (mk-pair (<= trc_size (+ " ^ (string_of_int gamma) ^ "
         mtb ) ) (evalb" ^ id ^ " trc mt mtb FVSYMBOL )) ) *)
      define_fun
        (* id *)
        (Id.mk Id.term ("evalc" ^ id))
        (* arguments *)
        [ Term.colon (f_const_term "mt") (f_const_sort "Time")
        ; Term.colon (f_const_term "mtb") (f_const_sort "Time") ]
        (* return type *)
        (Term.apply (f_const_sort "Pair")
           [f_const_sort "Bool"; f_const_sort "Fourvalue"] )
        (* body *)
        (Term.apply (f_const_term "mk-pair")
           [ f_geq
               (f_sum (f_const_term freevariable) (f_const_term "mtb"))
               (f_const_term "trc_size")
           ; Term.apply
               (f_const_term ("evalb" ^ id))
               [ f_const_term "trc"
               ; f_const_term "mt"
               ; f_const_term "mtb"
               ; f_const_term "FVSYMBOL" ] ] )
        helper
    in
    evalb id ;
    evalc id ;
    (* (define-fun eventually_op" ^ id ^ " ((mt Time) (mtb Time) ) Threevalue
       (mapb3 (evalc" ^ id ^ " mt mtb )) ) *)
    define_fun
      (* id *)
      (Id.mk Id.term ("eventually_op" ^ id))
      (* arguments *)
      [ Term.colon (f_const_term "mt") (f_const_sort "Time")
      ; Term.colon (f_const_term "mtb") (f_const_sort "Time") ]
      (* return type *)
      (f_const_sort "Threevalue")
      (* body *)
      (Term.apply (f_const_term "mapb3")
         [ Term.apply
             (f_const_term ("evalc" ^ id))
             [f_const_term "mt"; f_const_term "mtb"] ] )
      helper
  in
  eventually_eq ("!" ^ string_of_int idx) sf1 ;
  (* "(eventually_op!" ^ (string_of_int idx) ^" (+ mt "^ (string_of_int
     (int_of_float gamma)) ^") mt t )" *)
  Term.apply
    (f_const_term ("eventually_op!" ^ string_of_int idx))
    [f_sum (f_const_term "mt") (f_const_term freevariable); f_const_term "mt"]

let synth_fm_aw_eq gamma sf1 helper =
  synth_fm_not
    (synth_fm_uless gamma f_tvtrue (synth_fm_not sf1 helper) helper)
    helper

(* synthesis of U= *)
let synth_fm_ueq gamma sf1 sf2 helper =
  let x = synth_fm_aw_eq gamma sf1 helper in
  let x2 = synth_fm_ev_eq gamma sf2 helper in
  (* synth_fm_ueq is equal to synth_fm_aw_eq /\ synth_fm_ev_eq *)
  f_tvand x x2

let synth_fm_sless gamma sf1 sf2 helper =
  failwith ("S[<" ^ string_of_float gamma ^ "] Not Implemented!")

let synth_fm_seq gamma sf1 sf2 helper =
  failwith ("S[=" ^ string_of_float gamma ^ "] Not Implemented!")

let synth_smtlib_header helper =
  let common_header () =
    set_info ":smt-lib-version" (Term.real "2.6") helper
  in
  let common_header_cvc4 () =
    if recursive_unrolling helper then set_logic "QF_AUFDTNIRA" helper
    else set_logic "AUFDTNIRA" helper ;
    set_info_quoted ":source" "https://github.com/anmaped/rmtld3synth" helper ;
    set_info_str ":license" "https://creativecommons.org/licenses/by/4.0/"
      helper ;
    (* (set-info :category <category>) (set-info :status <status>) *)
    set_option_bool ":produce-models" "true" helper
  in
  let common_header_z3 () =
    set_option_bool ":auto_config" "false" helper ;
    set_option_bool ":model.v2" "true" helper ;
    set_option_int ":smt.phase_selection" "0" helper ;
    set_option_int ":smt.restart_strategy" "0" helper ;
    set_option_real ":smt.restart_factor" "1.5" helper ;
    set_option_bool ":smt.arith.random_initial_value" "true" helper ;
    set_option_int ":smt.case_split" "10" helper ;
    set_option_bool ":smt.delay_units" "true" helper ;
    set_option_int ":smt.delay_units_threshold" "300" helper ;
    set_option_int ":smt.qi.eager_threshold" "400" helper
  in
  common_header () ;
  common_header_cvc4 () ;
  if isZ3SolverEnabled helper then common_header_z3 () ;
  ()

let synth_smtlib_common_types helper =
  (* (define-sort Proptype () Int) *)
  define_sort (Id.mk Id.sort "Proptype") [] (f_const_sort "Int") helper ;
  define_sort (Id.mk Id.sort "Time") [] (f_const_sort "Int") helper ;
  define_sort (Id.mk Id.sort "Index") [] (f_const_sort "Int") helper ;
  (* (declare-datatypes ((Duration 0)) (( (dnone) (dsome (val Int) ) )) ) *)
  define_datatypes
    [ ( Id.mk Id.sort "Duration"
      , []
      , [ (Id.mk Id.term "dnone", [])
        ; ( Id.mk Id.term "dsome"
          , [Term.colon (f_const_term "dval") (f_const_sort "Int")] ) ] ) ]
    helper ;
  (* (declare-datatypes (T1 T2) ((Pair (mk-pair (first T1) (second T2)))))
     --> (declare-datatypes ( (Pair 2) ) ((par (T1 T2) ( (mk-pair (first T1)
     (second T2)) )))) *)
  define_datatypes
    [ ( Id.mk Id.sort "Pair"
      , [Term.const (Id.mk Id.attr "X"); Term.const (Id.mk Id.attr "Y")]
      , [ ( Id.mk Id.term "mk-pair"
          , [ Term.colon
                (Term.const (Id.mk Id.attr "first"))
                (Term.const (Id.mk Id.attr "X"))
            ; Term.colon
                (Term.const (Id.mk Id.attr "second"))
                (Term.const (Id.mk Id.attr "Y")) ] ) ] ) ]
    helper ;
  (* (define-sort Trace () (Array Index Proptype) ) *)
  define_sort (Id.mk Id.sort "Trace") []
    (Term.apply (f_const_sort "Array")
       [f_const_sort "Index"; f_const_sort "Proptype"] )
    helper ;
  (* (define-sort Trace_ () (Array Index Real) ) *)
  define_sort (Id.mk Id.sort "Trace_") []
    (Term.apply (f_const_sort "Array")
       [f_const_sort "Index"; f_const_sort "Real"] )
    helper ;
  (* (declare-datatypes () ((Fourvalue (FVTRUE) (FVFALSE) (FVUNKNOWN)
     (FVSYMBOL) ))) *)
  define_datatypes
    [ ( Id.mk Id.sort "Fourvalue"
      , []
      , [ (Id.mk Id.term "FVTRUE", [])
        ; (Id.mk Id.term "FVFALSE", [])
        ; (Id.mk Id.term "FVUNKNOWN", [])
        ; (Id.mk Id.term "FVSYMBOL", []) ] ) ]
    helper ;
  (* (declare-datatypes () ((Threevalue (TVTRUE) (TVFALSE) (TVUNKNOWN) ))) *)
  define_datatypes
    [ ( Id.mk Id.sort "Threevalue"
      , []
      , [ (Id.mk Id.term "TVTRUE", [])
        ; (Id.mk Id.term "TVFALSE", [])
        ; (Id.mk Id.term "TVUNKNOWN", []) ] ) ]
    helper ;
  (* (define-fun tvnot ((phi Threevalue)) Threevalue (ite (= phi TVTRUE)
     TVFALSE (ite (= phi TVFALSE) TVTRUE TVUNKNOWN) ) ) *)
  define_fun
    (* id *)
    (Id.mk Id.term "tvnot")
    (* arguments *)
    [Term.colon (f_const_term "phi") (f_const_sort "Threevalue")]
    (* return type *)
    (f_const_sort "Threevalue")
    (* body *)
    (ite
       (f_equal (f_const_term "phi") (f_const_term "TVTRUE"))
       (f_const_term "TVFALSE")
       (ite
          (f_equal (f_const_term "phi") (f_const_term "TVFALSE"))
          (f_const_term "TVTRUE")
          (f_const_term "TVUNKNOWN") ) )
    helper ;
  (* (define-fun tvor ((phi1 Threevalue) (phi2 Threevalue)) Threevalue (ite
     (or (= phi1 TVTRUE) (= phi2 TVTRUE) ) TVTRUE (ite (and (= phi1 TVFALSE)
     (= phi2 TVFALSE)) TVFALSE TVUNKNOWN ) ) ) *)
  define_fun
    (* id *)
    (Id.mk Id.term "tvor")
    (* arguments *)
    [ Term.colon (f_const_term "phi1") (f_const_sort "Threevalue")
    ; Term.colon (f_const_term "phi2") (f_const_sort "Threevalue") ]
    (* return type *)
    (f_const_sort "Threevalue")
    (* body *)
    (ite
       (f_or
          (f_equal (f_const_term "phi1") (f_const_term "TVTRUE"))
          (f_equal (f_const_term "phi2") (f_const_term "TVTRUE")) )
       (f_const_term "TVTRUE")
       (ite
          (f_and
             (f_equal (f_const_term "phi1") (f_const_term "TVFALSE"))
             (f_equal (f_const_term "phi2") (f_const_term "TVFALSE")) )
          (f_const_term "TVFALSE")
          (f_const_term "TVUNKNOWN") ) )
    helper ;
  (* (define-fun tvlessthan ((eta1 Duration) (eta2 Duration)) Threevalue (ite
     (or (= eta1 dnone) (= eta2 dnone)) TVUNKNOWN (ite (< (dval eta1) (dval
     eta2) ) TVTRUE TVFALSE) ) ) *)
  define_fun
    (* id *)
    (Id.mk Id.term "tvlessthan")
    (* arguments *)
    [ Term.colon (f_const_term "eta1") (f_const_sort "Duration")
    ; Term.colon (f_const_term "eta2") (f_const_sort "Duration") ]
    (* return type *)
    (f_const_sort "Threevalue")
    (* body *)
    (ite
       (f_or
          (f_equal (f_const_term "eta1") (f_const_term "dnone"))
          (f_equal (f_const_term "eta2") (f_const_term "dnone")) )
       (f_const_term "TVUNKNOWN")
       (ite
          (f_less
             (Term.apply (f_const_term "dval") [f_const_term "eta1"])
             (Term.apply (f_const_term "dval") [f_const_term "eta2"]) )
          (f_const_term "TVTRUE") (f_const_term "TVFALSE") ) )
    helper ;
  ()

let synth_smtlib_common_macros helper =
  (* (define-fun mapb4 ( (phi Threevalstlue) ) Fourvalue (ite (= phi TVTRUE )
     FVTRUE (ite (= phi TVFALSE ) FVFALSE FVUNKNOWN ) ) ) *)
  define_fun
    (* id *)
    (Id.mk Id.term "mapb4")
    (* arguments *)
    [Term.colon (f_const_term "phi") (f_const_sort "Threevalue")]
    (* return type *)
    (f_const_sort "Fourvalue")
    (* body *)
    (ite
       (f_equal (f_const_term "phi") (f_const_term "TVTRUE"))
       (f_const_term "FVTRUE")
       (ite
          (f_equal (f_const_term "phi") (f_const_term "TVFALSE"))
          (f_const_term "FVFALSE")
          (f_const_term "FVUNKNOWN") ) )
    helper ;
  (* (define-fun mapb3 ( (p (Pair Bool Fourvalue)) ) Threevalue

     (ite (= (second p) FVTRUE) TVTRUE (ite (= (second p) FVSYMBOL ) (ite
     (first p) TVUNKNOWN TVFALSE) (ite (= (second p) FVFALSE ) TVFALSE
     TVUNKNOWN ) ) ) ) *)
  define_fun
    (* id *)
    (Id.mk Id.term "mapb3")
    (* arguments *)
    [ Term.colon (f_const_term "p")
        (Term.apply (f_const_sort "Pair")
           [f_const_sort "Bool"; f_const_sort "Fourvalue"] ) ]
    (* return type *)
    (f_const_sort "Threevalue")
    (* body *)
    (ite
       (f_equal
          (Term.apply
             (Term.const (Id.mk Id.attr "second"))
             [f_const_term "p"] )
          (f_const_term "FVTRUE") )
       (f_const_term "TVTRUE")
       (ite
          (f_equal
             (Term.apply
                (Term.const (Id.mk Id.attr "second"))
                [f_const_term "p"] )
             (f_const_term "FVSYMBOL") )
          (ite
             (Term.apply
                (Term.const (Id.mk Id.attr "first"))
                [f_const_term "p"] )
             (f_const_term "TVUNKNOWN")
             (f_const_term "TVFALSE") )
          (ite
             (f_equal
                (Term.apply
                   (Term.const (Id.mk Id.attr "second"))
                   [f_const_term "p"] )
                (f_const_term "FVFALSE") )
             (f_const_term "TVFALSE")
             (f_const_term "TVUNKNOWN") ) ) )
    helper ;
  ()

let synth_smtlib_common_evali helper =
  (* (define-fun evali ((b1 Threevalue) (b2 Threevalue)) Fourvalue (ite (= b2
     TVFALSE) (ite (= b1 TVTRUE) FVSYMBOL (mapb4 b1) ) (mapb4 b2) ) ) *)
  define_fun
    (* id *)
    (Id.mk Id.term "evali")
    (* arguments *)
    [ Term.colon (f_const_term "b1") (f_const_sort "Threevalue")
    ; Term.colon (f_const_term "b2") (f_const_sort "Threevalue") ]
    (* return type *)
    (f_const_sort "Fourvalue")
    (* body *)
    (ite
       (f_equal (f_const_term "b2") (f_const_term "TVFALSE"))
       (ite
          (f_equal (f_const_term "b1") (f_const_term "TVTRUE"))
          (f_const_term "FVSYMBOL")
          (Term.apply (f_const_term "mapb4") [f_const_term "b1"]) )
       (Term.apply (f_const_term "mapb4") [f_const_term "b2"]) )
    helper ;
  ()

let synth_smtlib_common_trace helper =
  (* (declare-const trc Trace ) (declare-const trc_size Time) *)
  declare_const (Id.mk Id.term "trc") (f_const_sort "Trace") helper ;
  if not (assume_unary_sequence helper) then
    declare_const (Id.mk Id.term "trc_time") (f_const_sort "Trace_") helper ;
  declare_const (Id.mk Id.term "trc_size") (f_const_sort "Time") helper ;
  if not (assume_unary_sequence helper) then (
    declare_fun
      (* id *)
      (Id.mk Id.term "mapt")
      (* arguments *)
      [f_const_sort "Index"]
      (* return type *)
      (f_const_sort "Real")
      helper ;
    if recursive_unrolling helper then (
      (* unrooling the recursion just in case (speedup) *)
      let enumeration = of_enum 0 (recursive_unrolling_depth helper) in
      (*let lst_all_comb = cartesian enumeration enumeration in*)
      List.fold_left
        (fun _ i ->
          if 0 < i then
            assert_
              (f_equal
                 (Term.apply (f_const_term "mapt")
                    [Term.int (string_of_int i)] )
                 (f_sum
                    (Term.apply (f_const_term "mapt")
                       [f_minus (Term.int (string_of_int i)) (Term.int "1")] )
                    (Term.apply (f_const_term "select")
                       [f_const_term "trc_time"; Term.int (string_of_int i)] ) ) )
              helper
          else
            assert_
              (f_equal
                 (Term.apply (f_const_term "mapt")
                    [Term.int (string_of_int i)] )
                 (Term.apply (f_const_term "select")
                    [f_const_term "trc_time"; Term.int (string_of_int i)] ) )
              helper )
        () enumeration ;
      assert_
        (f_equal
           (Term.apply (f_const_term "mapt")
              [f_minus (Term.int "0") (Term.int "1")] )
           (Term.real "0") )
        helper
      (* (assert (= (select trc_time 0) 0 )) *)
      (*assert_ lst (f_equal (f_const_term "0") (Term.apply (f_const_term
        "select") [ f_const_term "trc_time"; f_const_term "0" ]) ) ;*) )
    else
      assert_
        (Term.forall
           [Term.colon (f_const_term "i") (f_const_sort "Index")]
           (ite
              (f_less (Term.int "0") (f_const_term "i"))
              (f_equal
                 (Term.apply (f_const_term "mapt") [f_const_term "i"])
                 (f_sum
                    (Term.apply (f_const_term "mapt")
                       [f_minus (f_const_term "i") (Term.int "1")] )
                    (Term.apply (f_const_term "select")
                       [f_const_term "trc_time"; f_const_term "i"] ) ) )
              (f_equal
                 (Term.apply (f_const_term "mapt") [f_const_term "i"])
                 (Term.apply (f_const_term "select")
                    [f_const_term "trc_time"; f_const_term "i"] ) ) ) )
        helper ) ;
  ()

let synth_smtlib_common_prop helper =
  (* (define-fun ev_prop ( (mk Trace) (mt Time) (p Proptype) ) Threevalue
     (ite (>= trc_size mt) (ite (= (select mk mt) p) TVTRUE TVFALSE )
     TVUNKNOWN ) ) *)
  define_fun
    (* id *)
    (Id.mk Id.term "ev_prop")
    (* arguments *)
    [ Term.colon (f_const_term "mk") (f_const_sort "Trace")
    ; Term.colon (f_const_term "mt") (f_const_sort "Time")
    ; Term.colon (f_const_term "p") (f_const_sort "Proptype") ]
    (* return type *)
    (f_const_sort "Threevalue")
    (* body *)
    (ite
       (f_geq (f_const_term "trc_size") (f_const_term "mt"))
       (ite
          (f_equal
             (Term.apply (f_const_term "select")
                [f_const_term "mk"; f_const_term "mt"] )
             (f_const_term "p") )
          (f_const_term "TVTRUE") (f_const_term "TVFALSE") )
       (f_const_term "TVUNKNOWN") )
    helper ;
  ()

let synth_smtlib' fmt synth_fun formula helper =
  synth_smtlib_header helper ;
  synth_smtlib_common_types helper ;
  synth_smtlib_common_macros helper ;
  synth_smtlib_common_evali helper ;
  synth_smtlib_common_trace helper ;
  synth_smtlib_common_prop helper ;
  let tm = synth_fun formula helper in
  (* (define-fun allcheck ((mk Trace) (mt Time)) Bool (= "^ tm ^" TVTRUE)
     ) *)
  define_fun
    (* id *)
    (Id.mk Id.term "allcheck")
    (* arguments *)
    [ Term.colon (f_const_term "mk") (f_const_sort "Trace")
    ; Term.colon (f_const_term "mt") (f_const_sort "Time") ]
    (* return type *)
    (f_const_sort "Bool")
    (* body *)
    (f_equal tm (f_const_term "TVTRUE"))
    helper ;
  (* if isZ3SolverEnabled helper then (assert (forall ((t Time)) (>= (select
     trc t) 0) )) *)
  if isZ3SolverEnabled helper then
    assert_
      (Term.forall
         [Term.colon (f_const_term "t") (f_const_sort "Time")]
         (f_geq
            (Term.apply (f_const_term "select")
               [f_const_term "trc"; f_const_term "t"] )
            (Term.int "0") ) )
      helper ;
  if not (assume_unary_sequence helper) then
    assert_
      (Term.forall
         [Term.colon (f_const_term "t") (f_const_sort "Time")]
         (f_less (Term.int "0")
            (Term.apply (f_const_term "select")
               [f_const_term "trc_time"; f_const_term "t"] ) ) )
      helper ;
  (* (assert (allcheck trc 0) ) *)
  add
    (Statement.assert_
       (Term.apply (f_const_term "allcheck")
          [f_const_term "trc"; Term.int "0"] ) )
    helper ;
  if not (isZ3SolverEnabled helper) then (
    check_sat helper ;
    get_model helper ;
    get_info ":all-statistics" helper ) ;
  (* if isZ3SolverEnabled helper then "(check-sat-using (then qe smt))" ;
     this entry is available only in Z3 *)
  (* 
   * initialize dolmen to pretty print smtlib
   *)
  let lang : Dolmen_loop.Logic.language = Smtlib2 `V2_6 in
  let logic_file = State.mk_file ~lang "./" (`File "this is unused") in
  let response_file = State.mk_file "" (`File "this is unused") in
  let st =
    State.empty
    |> State.init ~debug:false ~report_style:Contextual ~max_warn:max_int
         ~reports:(Dolmen_loop.Report.Conf.mk ~default:Enabled)
         ~response_file ~time_limit:0. ~size_limit:0.
    |> State.set State.logic_file logic_file
    |> Typer.init ~smtlib2_forced_logic:(Some "AUFDTNIRA")
    |> Typer_Pipe.init ~type_check:true
  in
  let acc =
    Export.Smtlib2_6.init ~close:(fun _ -> ()) Format.str_formatter
  in
  (* 
   * pretty print smtlib statements to string
   *)
  let lst = get_variable_lst helper |> List.rev in
  let _, _ =
    List.fold_left
      (fun (st, acc) a ->
        verb_m 2 (fun _ ->
            Statement.print Format.std_formatter a ;
            Format.pp_print_newline Format.std_formatter () ) ;
        (* print smtlib *)
        let st, res = Typer_Pipe.typecheck st a in
        let stmt =
          match res with
          | `Continue [stmt] -> stmt
          | `Continue [] -> failwith "No typechecked statement returned"
          | `Continue _ ->
              failwith "Multiple typechecked statements returned"
          | `Done () -> failwith "Typechecking returned `Done ()"
        in
        let st, acc = Export.Smtlib2_6.print st acc stmt in
        (st, acc) )
      (st, acc) lst
  in
  let output_str : string = Format.flush_str_formatter () in
  let hexid = String.sub (Digest.string output_str |> Digest.to_hex) 0 4 in
  try
    let out_dir = get_setting_string "out_dir" helper in
    verbose (pp_endline fmt) "Generated Output Files:" ;
    let name = String.capitalize_ascii ("encoding_" ^ hexid) in
    save_file (out_dir ^ "/" ^ name ^ ".smt2") output_str ;
    verbose (pp_endline fmt) (out_dir ^ "/" ^ name ^ ".smt2") ;
    output_str
  with Not_found -> (
    try
      ( if
          Filename.check_suffix
            (get_setting_string "out_file" helper)
            ".smt2"
        then failwith "Output file name must end with .smt2 suffix."
        else
          let out_file = get_setting_string "out_file" helper in
          save_file out_file output_str ;
          verbose (pp_endline fmt) ("SMTLIBv2 file " ^ out_file ^ " saved.")
      ) ;
      if isZ3SolverEnabled helper then output_str else ""
    with Not_found ->
      if not (isZ3SolverEnabled helper) then pp_endline fmt output_str ;
      output_str )

let synth_smtlib fmt synth_fun helper =
  let pp_endline = pp_endline fmt in
  init_set_variables helper ;
  (* list of expressions with input_exp tag *)
  let input_lst = get_all_setting_formula "input_exp" helper in
  (* contains at least one formula *)
  if List.length input_lst = 0 then
    failwith
      "No input formula provided! Please provide at least one formula with \
       the 'input_exp' tag."
  else (
    verbose pp_endline
      ( "Number of input formula(s) provided: "
      ^ string_of_int (List.length input_lst) ) ;
    List.mapi
      (fun idx fm ->
        set_recursive_unrolling_depth fm helper ;
        verbose pp_endline
          ( "Setting recursive unrolling depth to "
          ^ string_of_int (get_setting_int "rec_unrolling_depth" helper) ) ;
        verbose pp_endline
          ( "Processing input formula "
          ^ string_of_int (idx + 1)
          ^ "/"
          ^ string_of_int (List.length input_lst) ) ;
        (string_of_int idx ^ ".smt2", synth_smtlib' fmt synth_fun fm helper)
        )
      input_lst )
