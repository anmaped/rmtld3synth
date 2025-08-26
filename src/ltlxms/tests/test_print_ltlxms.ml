(* It provides a command-line interface to print json files corresponding to
   formulas defined in LTLxMS format. *)
open Ltlxms.Syntax

let () =
  (* Construct the formula *)
  (* test 4 *)
  let test_4 =
    And
      ( SubSetEq (Proposition "a", Proposition "b")
      , Next (Next (SubSetEq (Proposition "a", Proposition "b"))) )
  in
  (* property 1 *)
  let property_1 =
    Until
      ( Disconnected (Proposition "C1", Proposition "C2")
      , Overlap (Proposition "C1", Proposition "C2") )
  in
  (* property 3 *)
  (* (always (or (overlap (expand (prop "a") 1.) (prop "b")) (eventually
     (subseteq (prop "a") (prop "b"))))) *)
  let property_3 =
    Always
      (Or
         ( Overlap (Expand (Proposition "a", 1.), Proposition "b")
         , Eventually (SubSetEq (Proposition "a", Proposition "b")) ) )
  in
  let print fm =
    (* Convert the formula to JSON *)
    let json = Ltlxms.Syntax.yojson_of_formula fm in
    (* Print the JSON *)
    Printf.printf "Expected JSON: %s\n" (Yojson.Safe.pretty_to_string json)
  in
  print test_4 ; print property_1 ; print property_3
