(* This module defines the syntax for terms and formulas used in LTLXMS. It
   includes types for propositions, logical operations, and temporal
   operators, along with their JSON serialization and deserialization. *)

open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type term =
  | Proposition of string
  | Intersection of term * term
  | Union of term * term
  | Negation of term
  | NextT of term
  | Expand of term * float
  | UntilT of term * term
[@@deriving yojson]

type formula =
  | SubSetEq of term * term
  | Collides of term * term
  | Equal of term * term
  | Disconnected of term * term
  | And of formula * formula
  | Or of formula * formula
  | Not of formula
  | Next of formula
  | Until of formula * formula
  | Always of formula
  | Eventually of formula
  | Previous of formula
  | Since of formula * formula
  | Implies of formula * formula
  | Overlap of term * term
  | Include of term * term
  | AlwaysPast of formula
  | Once of formula
[@@deriving yojson]
