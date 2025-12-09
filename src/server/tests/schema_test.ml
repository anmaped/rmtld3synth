(* Schema validation tests using ocaml jsonschema *)

(** Read and parse JSON from a file, failing with an error message if parsing fails. *)
let read_json_from_file file_path =
  try Yojson.Basic.from_file file_path
  with
  | Yojson.Json_error msg ->
      failwith (Printf.sprintf "Failed to parse JSON from %s: %s" file_path msg)
  | Sys_error msg ->
      failwith (Printf.sprintf "Failed to read file %s: %s" file_path msg)

(** The schema loaded from schema.json for validation. *)
let schema_json = lazy (read_json_from_file "../schema.json")
let schema () = Yojson.Basic.to_string (Lazy.force schema_json)

(** Validate a JSON file against the schema, failing the test if validation fails. *)
let validate_schema_file json_file =
  let json = Yojson.Basic.to_string (read_json_from_file json_file) in
  Jsonschema.validate_strings ~schema:(schema ()) ~json ()

(** Assert that a function raises an exception. *)
let expect_failure f =
  match f () with
  | Ok () -> Alcotest.fail "Expected validation to fail, but it succeeded"
  | Error _ | exception _ -> ()

(** Assert that a function succeeds without exception. *)
let expect_success f =
  match f () with
  | Ok () -> ()
  | Error msg ->
      Alcotest.fail ("Expected validation to succeed, but got error: " ^ (Jsonschema.Validation_error.to_string msg) )
  | exception ex ->
      Alcotest.fail ("Expected validation to succeed, but got exception: " ^ (Printexc.to_string ex))

(** Validate one file, asserting success or failure based on filename suffix. *)
let validate_schema_file_expected file =
  let should_fail = Filename.check_suffix file ".fail.in" in
  if should_fail then
    expect_failure (fun () -> validate_schema_file file)
  else
    expect_success (fun () -> validate_schema_file file)

(** Run all schema validation tests. *)
let () =
  let test_dir = "." in
  let test_files =
    Sys.readdir test_dir
    |> Array.to_list
    |> List.filter (fun name ->
           Filename.check_suffix name ".in" && name <> "schema.json")
    |> List.sort String.compare
  in
  let test_cases =
    List.map
      (fun file ->
        Alcotest.test_case ("validate " ^ file) `Quick (fun () ->
            validate_schema_file_expected file))
      test_files
  in
  Alcotest.run "Schema Validation" [ ("validation", test_cases) ]
