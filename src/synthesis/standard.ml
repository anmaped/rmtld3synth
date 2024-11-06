open Sexplib
open Rmtld3
open Helper

type prop_base10 = int

module type Translate_sig = sig
  type body

  val synth_tm_constant : value -> helper -> body

  val synth_tm_variable : string -> helper -> body

  val synth_tm_duration : body -> body -> helper -> body

  val synth_tm_plus : body -> body -> helper -> body

  val synth_tm_times : body -> body -> helper -> body

  val synth_fm_true : helper -> body

  val synth_fm_p : prop_base10 -> helper -> body

  val synth_fm_not : body -> helper -> body

  val synth_fm_or : body -> body -> helper -> body

  val synth_fm_less : body -> body -> helper -> body

  val synth_fm_uless : value -> body -> body -> helper -> body

  val synth_fm_ueq : value -> body -> body -> helper -> body

  val synth_fm_sless : value -> body -> body -> helper -> body

  val synth_fm_seq : value -> body -> body -> helper -> body
end

module Translate (T : Translate_sig) = struct
  (* Synthesis of the rmtld3 terms *)
  let rec synth_term term helper =
    match term with
    | Constant value -> T.synth_tm_constant value helper
    | Variable name -> T.synth_tm_variable name helper
    | Duration (di, phi) ->
        T.synth_tm_duration (synth_term di helper) (synth phi helper) helper
    | FPlus (tr1, tr2) ->
        T.synth_tm_plus (synth_term tr1 helper) (synth_term tr2 helper)
          helper
    | FTimes (tr1, tr2) ->
        T.synth_tm_times (synth_term tr1 helper) (synth_term tr2 helper)
          helper

  (* Synthesis of the rmtld3 formulas *)
  and synth formula helper =
    match formula with
    | True () -> T.synth_fm_true helper
    | Prop p ->
        find_proposition_hashtbl p helper
        |> (function
             | Some cnt -> cnt
             | None ->
           let cnt = get_proposition_counter helper in
           set_proposition_two_way_map p cnt helper;
           cnt)
        |> fun cnt -> T.synth_fm_p cnt helper
    | Not sf -> T.synth_fm_not (synth sf helper) helper
    | Or (sf1, sf2) ->
        T.synth_fm_or (synth sf1 helper) (synth sf2 helper) helper
    | Until (gamma, sf1, sf2) ->
        if gamma > 0. then
          T.synth_fm_uless gamma (synth sf1 helper) (synth sf2 helper) helper
        else raise (Failure "Gamma of U< operator is negative")
    | Since (gamma, sf1, sf2) ->
        if gamma > 0. then
          T.synth_fm_sless gamma (synth sf1 helper) (synth sf2 helper) helper
        else raise (Failure "Gamma of S< operator is negative")
    | Until_eq (gamma, sf1, sf2) ->
        if gamma > 0. then
          T.synth_fm_ueq gamma (synth sf1 helper) (synth sf2 helper) helper
        else raise (Failure "Gamma of U= operator is negative")
    | Since_eq (gamma, sf1, sf2) ->
        if gamma > 0. then
          T.synth_fm_seq gamma (synth sf1 helper) (synth sf2 helper) helper
        else raise (Failure "Gamma of S= operator is negative")
    | LessThan (tr1, tr2) ->
        T.synth_fm_less (synth_term tr1 helper) (synth_term tr2 helper)
          helper
    | _ ->
        raise
          (Failure
             ( "synth: unsupported formula "
             ^ Sexp.to_string_hum (sexp_of_rmtld3_fm formula) ) )
end
