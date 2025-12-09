(* JSON schema validation using jsonschema *)

(** Default JSON schema for RMTLD tool configuration *)
let schema_json : string = [%blob "schema.json"]

(** Validate JSON value against schema loaded from file *)
let validate_schema_file ~schema_file json =
  match Jsonschema.validate_file ~schema:schema_file json with
  | Ok () -> Ok ()
  | Error err -> Error (Jsonschema.Validation_error.to_string err)

(** Validate JSON value against schema from string *)
let validate_schema_string ~schema ~json =
  match Jsonschema.validate_strings ~schema ~json () with
  | Ok () -> Ok ()
  | Error err -> Error (Jsonschema.Validation_error.to_string err)

(** Read JSON value from file *)
let read_json_from_file file_path =
  try Yojson.Basic.from_file file_path
  with Yojson.Json_error msg ->
    failwith
      (Printf.sprintf "Failed to parse JSON from %s: %s" file_path msg)
