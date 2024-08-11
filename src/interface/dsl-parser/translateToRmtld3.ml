open Rmtld3
open Ast

let conv_u = function
  | NoUnits | S -> 1
  | Ms -> 1_000
  | Us -> 1_000_000
  | Ns -> 1_000_000_000

let conv_time = function Unbound -> max_int | Bound x -> x

let conv_kind = function
  | Less (Unbound, NoUnits) -> max_float
  | Less (t, u) | Equal (t, u) | LessOrEqual (t, u) ->
      (conv_time t |> float_of_int) /. (conv_u u |> float_of_int)

let rec conv_tm : tm -> Rmtld3.tm = function
  | Constant x -> Constant x
  | C (t, u) ->
      Constant ((conv_time t |> float_of_int) /. (conv_u u |> float_of_int))
  | FPlus (a, b) -> FPlus (conv_tm a, conv_tm b)
  | FTimes (a, b) -> FTimes (conv_tm a, conv_tm b)
  | Duration (i, f) -> Duration (conv_interval i, conv_fm f)

and conv_interval : interval -> Rmtld3.tm = function
  | Interval (a, b) when conv_tm a = Constant 0. -> conv_tm b
  | _ -> failwith "interval does not start at 0.!"

and conv_fm : fm -> Rmtld3.fm = function
  | True -> mtrue
  | False -> mfalse
  | Not a -> Not (conv_fm a)
  | Prop c -> Prop c
  | Or (a, b) -> Or (conv_fm a, conv_fm b)
  | And (a, b) -> mand (conv_fm a) (conv_fm b)
  | Implies (a, b) -> mimplies (conv_fm a) (conv_fm b)
  | LessThan (a, b) -> LessThan (conv_tm a, conv_tm b)
  | LessOrEqualThan (a, b) -> less_or_equal (conv_tm a) (conv_tm b)
  | GreaterThan (a, b) -> greater (conv_tm a) (conv_tm b)
  | GreaterOrEqualThan (a, b) -> greater_or_equal (conv_tm a) (conv_tm b)
  | Until ((Less (t, u) as k), a, b) ->
      Until (conv_kind k, conv_fm a, conv_fm b)
  | Until ((Equal (t, u) as k), a, b) ->
      Until_eq (conv_kind k, conv_fm a, conv_fm b)
  | Until ((LessOrEqual (t, u) as k), a, b) ->
      Or
        ( Until (conv_kind k, conv_fm a, conv_fm b)
        , Until_eq (conv_kind k, conv_fm a, conv_fm b) )
  | Since ((Less (t, u) as k), a, b) ->
      Since (conv_kind k, conv_fm a, conv_fm b)
  | Since ((Equal (t, u) as k), a, b) ->
      Since_eq (conv_kind k, conv_fm a, conv_fm b)
  | Since ((LessOrEqual (t, u) as k), a, b) ->
      Or
        ( Since (conv_kind k, conv_fm a, conv_fm b)
        , Since_eq (conv_kind k, conv_fm a, conv_fm b) )
  | Rise (k, f) -> mand (mprev (conv_kind k) (conv_fm (Not(f)))) (conv_fm f)
  | Fall (k, f) -> mand (mprev (conv_kind k) (conv_fm f)) (conv_fm (Not(f)))
  | Next ((Less (t, u) as k), f) -> mnext (conv_kind k) (conv_fm f)
  | Next ((Equal (t, u) as k), f) -> mnext_eq (conv_kind k) (conv_fm f)
  | Next ((LessOrEqual (t, u) as k), f) ->
      mnext_leq (conv_kind k) (conv_fm f)
  | Prev ((Less (t, u) as k), f) -> mprev (conv_kind k) (conv_fm f)
  | Prev ((Equal (t, u) as k), f) -> mprev_eq (conv_kind k) (conv_fm f)
  | Prev ((LessOrEqual (t, u) as k), f) ->
      mprev_leq (conv_kind k) (conv_fm f)
  | Always ((Less (t, u) as k), f) -> malways (conv_kind k) (conv_fm f)
  | Always ((Equal (t, u) as k), f) -> malways_eq (conv_kind k) (conv_fm f)
  | Always ((LessOrEqual (t, u) as k), f) ->
      malways_leq (conv_kind k) (conv_fm f)
  | Historically ((Less (t, u) as k), f) ->
      mhistorically (conv_kind k) (conv_fm f)
  | Historically ((Equal (t, u) as k), f) ->
      mhistorically_eq (conv_kind k) (conv_fm f)
  | Historically ((LessOrEqual (t, u) as k), f) ->
      mhistorically_leq (conv_kind k) (conv_fm f)
  | Eventually ((Less (t, u) as k), f) ->
      meventually (conv_kind k) (conv_fm f)
  | Eventually ((Equal (t, u) as k), f) ->
      meventually_eq (conv_kind k) (conv_fm f)
  | Eventually ((LessOrEqual (t, u) as k), f) ->
      meventually_leq (conv_kind k) (conv_fm f)
  | PastEventually ((Less (t, u) as k), f) ->
      mpasteventually (conv_kind k) (conv_fm f)
  | PastEventually ((Equal (t, u) as k), f) ->
      mpasteventually_eq (conv_kind k) (conv_fm f)
  | PastEventually ((LessOrEqual (t, u) as k), f) ->
      mpasteventually_leq (conv_kind k) (conv_fm f)
