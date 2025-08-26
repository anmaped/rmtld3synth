type temporal_operator_decls =
  { next_decl: Z3.FuncDecl.func_decl
  ; nextt_decl: Z3.FuncDecl.func_decl
  ; previous_decl: Z3.FuncDecl.func_decl
  ; once_decl: Z3.FuncDecl.func_decl
  ; always_decl: Z3.FuncDecl.func_decl
  ; eventually_decl: Z3.FuncDecl.func_decl }

module Formulas = struct
  (* Module for basic logical operations *)
  module Logic = struct
    let single_or (ctx : Z3.context) (expr1 : Z3.Expr.expr)
        (expr2 : Z3.Expr.expr) : Z3.Expr.expr =
      Z3.Boolean.mk_or ctx [expr1; expr2]

    let single_and (ctx : Z3.context) (expr1 : Z3.Expr.expr)
        (expr2 : Z3.Expr.expr) : Z3.Expr.expr =
      Z3.Boolean.mk_and ctx [expr1; expr2]

    let single_not (ctx : Z3.context) (expr : Z3.Expr.expr) : Z3.Expr.expr =
      Z3.Boolean.mk_not ctx expr

    let single_implies (ctx : Z3.context) (expr1 : Z3.Expr.expr)
        (expr2 : Z3.Expr.expr) : Z3.Expr.expr =
      Z3.Boolean.mk_implies ctx expr1 expr2

    let sub_set_eq (ctx : Z3.context) (tau1 : Z3.Expr.expr)
        (tau2 : Z3.Expr.expr) : Z3.Expr.expr =
      let x = Z3.Arithmetic.Real.mk_const_s ctx "xs" in
      let y = Z3.Arithmetic.Real.mk_const_s ctx "ys" in
      let quantifier =
        Z3.Quantifier.mk_forall_const ctx [x; y]
          (Z3.Boolean.mk_implies ctx tau1 tau2)
          None [] [] None None
      in
      Z3.Quantifier.expr_of_quantifier quantifier

    let overlap (ctx : Z3.context) (expr1 : Z3.Expr.expr)
        (expr2 : Z3.Expr.expr) : Z3.Expr.expr =
      let symbol = Z3.Symbol.mk_string ctx "overlap" in
      let func_decl =
        Z3.FuncDecl.mk_func_decl ctx symbol
          [Z3.Boolean.mk_sort ctx; Z3.Boolean.mk_sort ctx]
          (Z3.Boolean.mk_sort ctx)
      in
      Z3.Expr.mk_app ctx func_decl [expr1; expr2]

    let list_and ctx exprs =
      match exprs with
      | [] -> Z3.Boolean.mk_true ctx
      | [e] -> e
      | _ -> Z3.Boolean.mk_and ctx exprs

    let list_or ctx exprs =
      match exprs with
      | [] -> Z3.Boolean.mk_false ctx
      | [e] -> e
      | _ -> Z3.Boolean.mk_or ctx exprs

    let circle_overlap ctx (x1, y1, r1) (x2, y2, r2) =
      let dx = Z3.Arithmetic.mk_sub ctx [x1; x2] in
      let dy = Z3.Arithmetic.mk_sub ctx [y1; y2] in
      let dist2 =
        Z3.Arithmetic.mk_add ctx
          [ Z3.Arithmetic.mk_mul ctx [dx; dx]
          ; Z3.Arithmetic.mk_mul ctx [dy; dy] ]
      in
      let rsum = Z3.Arithmetic.mk_add ctx [r1; r2] in
      let rsum2 = Z3.Arithmetic.mk_mul ctx [rsum; rsum] in
      Z3.Arithmetic.mk_le ctx dist2 rsum2
  end

  (* Module for temporal logic operations *)
  module Temporal = struct
    (* NextT operator *)
    let nextt (ctx : Z3.context) (decls : temporal_operator_decls)
        (formula : Z3.Expr.expr) : Z3.Expr.expr =
      Z3.Expr.mk_app ctx decls.nextt_decl [formula]

    (* Once operator *)
    let once (ctx : Z3.context) (decls : temporal_operator_decls)
        (formula : Z3.Expr.expr) : Z3.Expr.expr =
      Z3.Expr.mk_app ctx decls.once_decl [formula]

    (* Next operator *)
    let next (ctx : Z3.context) (decls : temporal_operator_decls)
        (formula : Z3.Expr.expr) : Z3.Expr.expr =
      Z3.Expr.mk_app ctx decls.next_decl [formula]

    (* Always operator *)
    let always (ctx : Z3.context) (decls : temporal_operator_decls)
        (formula : Z3.Expr.expr) : Z3.Expr.expr =
      Z3.Expr.mk_app ctx decls.always_decl [formula]

    (* Eventually operator *)
    let eventually (ctx : Z3.context) (decls : temporal_operator_decls)
        (formula : Z3.Expr.expr) : Z3.Expr.expr =
      Z3.Expr.mk_app ctx decls.eventually_decl [formula]

    (* Helper function to apply 'next' n times *)
    let rec apply_next_n_times (ctx : Z3.context)
        (decls : temporal_operator_decls) (expr : Z3.Expr.expr) (n : int) :
        Z3.Expr.expr =
      if n < 0 then
        invalid_arg
          "n cannot be negative for apply_next_n_times: n must be >= 0"
      else if n = 0 then expr
      else apply_next_n_times ctx decls (next ctx decls expr) (n - 1)

    (* Until operator: e1 U e2 unfolded "unravel" times *)
    let until (ctx : Z3.context) (decls : temporal_operator_decls)
        (e1 : Z3.Expr.expr) (e2 : Z3.Expr.expr) (unravel : int) :
        Z3.Expr.expr =
      if unravel < 0 then
        invalid_arg "Unravel depth for 'until' must be non-negative."
      else
        let rec generate_disjuncts_iter k acc_disjuncts =
          if k > unravel then List.rev acc_disjuncts
          else
            let term_e2_part = apply_next_n_times ctx decls e2 k in
            let rec build_e1_conjunction_list_iter current_j_for_e1
                acc_e1_terms_list =
              if current_j_for_e1 >= k then List.rev acc_e1_terms_list
              else
                let next_j_e1 =
                  apply_next_n_times ctx decls e1 current_j_for_e1
                in
                build_e1_conjunction_list_iter (current_j_for_e1 + 1)
                  (next_j_e1 :: acc_e1_terms_list)
            in
            let e1_terms_for_conjunction =
              build_e1_conjunction_list_iter 0 []
            in
            let current_disjunct_term =
              if k = 0 then term_e2_part
              else
                let all_components_for_and =
                  e1_terms_for_conjunction @ [term_e2_part]
                in
                Z3.Boolean.mk_and ctx all_components_for_and
            in
            generate_disjuncts_iter (k + 1)
              (current_disjunct_term :: acc_disjuncts)
        in
        let all_disjuncts_list = generate_disjuncts_iter 0 [] in
        Z3.Boolean.mk_or ctx all_disjuncts_list

    (* Previous operator*)
    let previous (ctx : Z3.context) (decls : temporal_operator_decls)
        (formula : Z3.Expr.expr) : Z3.Expr.expr =
      Z3.Expr.mk_app ctx decls.previous_decl [formula]

    (* Auxiliary function for the below since implementation *)
    let rec since_aux (ctx : Z3.context) (decls : temporal_operator_decls)
        (formula1 : Z3.Expr.expr) (formula2 : Z3.Expr.expr) (n : int) :
        Z3.Expr.expr =
      if n = 0 then Logic.single_and ctx formula1 formula2
      else if n > 0 then
        Logic.single_and ctx
          (previous ctx decls formula1)
          (since_aux ctx decls formula1 formula2 (n - 1))
      else previous ctx decls formula2

    let rec since_formula (ctx : Z3.context)
        (decls : temporal_operator_decls) (formula1 : Z3.Expr.expr)
        (formula2 : Z3.Expr.expr) (bound : int) (m : int) : Z3.Expr.expr =
      if m >= bound then Z3.Boolean.mk_false ctx
      else if m + 2 <= bound then
        Logic.single_or ctx
          (since_aux ctx decls formula1 formula2 (m + 1))
          (since_formula ctx decls formula1 formula2 bound (m + 1))
      else Z3.Boolean.mk_false ctx
  end

  open Syntax

  (* Convert a term into a Z3 expression *)
  let rec term_to_z3 ?(unwinding_depth=10) ctx (decls : temporal_operator_decls) term =
    match term with
    | Proposition p -> Z3.Boolean.mk_const ctx (Z3.Symbol.mk_string ctx p)
    | Intersection (t1, t2) ->
        Logic.single_and ctx (term_to_z3 ctx decls t1)
          (term_to_z3 ctx decls t2)
    | Union (t1, t2) ->
        Logic.single_or ctx (term_to_z3 ctx decls t1)
          (term_to_z3 ctx decls t2)
    | Expand (t, value) ->
        let expand_symbol = Z3.Symbol.mk_string ctx "expand" in
        let expand_func =
          Z3.FuncDecl.mk_func_decl ctx expand_symbol
            [Z3.Boolean.mk_sort ctx; Z3.Arithmetic.Real.mk_sort ctx]
            (Z3.Boolean.mk_sort ctx)
        in
        Z3.Expr.mk_app ctx expand_func
          [ term_to_z3 ctx decls t
          ; Z3.Arithmetic.Real.mk_numeral_s ctx (string_of_float value) ]
    | NextT t -> Temporal.nextt ctx decls (term_to_z3 ctx decls t)
    | UntilT (t1, t2) ->
        let e1 = term_to_z3 ctx decls t1 in
        let e2 = term_to_z3 ctx decls t2 in
        Temporal.until ctx decls e1 e2 unwinding_depth
    | Negation t -> Logic.single_not ctx (term_to_z3 ctx decls t)

  (* Convert a formula into a Z3 expression *)
  let rec formula_to_z3 ?(unwinding_depth=10) ctx (decls : temporal_operator_decls) formula =
    match formula with
    | SubSetEq (t1, t2) ->
        Logic.sub_set_eq ctx (term_to_z3 ctx decls t1)
          (term_to_z3 ctx decls t2)
    | Collides (t1, t2) ->
        let disc = Disconnected (t1, t2) in
        let over = Overlap (t1, t2) in
        formula_to_z3 ctx decls (Until (disc, over))
    | Equal (t1, t2) ->
        let sub1 =
          Logic.sub_set_eq ctx (term_to_z3 ctx decls t1)
            (term_to_z3 ctx decls t2)
        in
        let sub2 =
          Logic.sub_set_eq ctx (term_to_z3 ctx decls t2)
            (term_to_z3 ctx decls t1)
        in
        Logic.single_and ctx sub1 sub2
    | Disconnected (t1, t2) ->
        let not_t2 = Negation t2 in
        let not_t1 = Negation t1 in
        let sub1 =
          Logic.sub_set_eq ctx (term_to_z3 ctx decls t1)
            (term_to_z3 ctx decls not_t2)
        in
        let sub2 =
          Logic.sub_set_eq ctx (term_to_z3 ctx decls t2)
            (term_to_z3 ctx decls not_t1)
        in
        Logic.single_and ctx sub1 sub2
    | And (f1, f2) ->
        Logic.single_and ctx
          (formula_to_z3 ctx decls f1)
          (formula_to_z3 ctx decls f2)
    | Or (f1, f2) ->
        Logic.single_or ctx
          (formula_to_z3 ctx decls f1)
          (formula_to_z3 ctx decls f2)
    | Not f -> Logic.single_not ctx (formula_to_z3 ctx decls f)
    | Next f -> Temporal.next ctx decls (formula_to_z3 ctx decls f)
    | Always f -> Temporal.always ctx decls (formula_to_z3 ctx decls f)
    | Eventually f ->
        Temporal.eventually ctx decls (formula_to_z3 ctx decls f)
    | Once f -> Temporal.once ctx decls (formula_to_z3 ctx decls f)
    | Overlap (t1, t2) ->
        Logic.overlap ctx (term_to_z3 ctx decls t1) (term_to_z3 ctx decls t2)
    | Implies (f1, f2) ->
        Logic.single_implies ctx
          (formula_to_z3 ctx decls f1)
          (formula_to_z3 ctx decls f2)
    | Previous f -> Temporal.previous ctx decls (formula_to_z3 ctx decls f)
    | Until (f1, f2) ->
        let e1 = formula_to_z3 ctx decls f1 in
        let e2 = formula_to_z3 ctx decls f2 in
        Temporal.until ctx decls e1 e2 unwinding_depth
    | _ -> failwith "Unsupported formula type for conversion to Z3"
end
