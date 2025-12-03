(* Schema validation tests using ocaml-jsonschema *)

(** Default JSON schema for RMTLD tool configuration *)
let schema_json =
  {|{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "title": "RMTLD Tool Configuration",
  "type": "object",
  "properties": {
    "gen_rmtld_formula": { "type": "boolean", "default": false },
    "synth_smtlibv2":    { "type": "boolean", "default": false },
    "synth_ocaml":       { "type": "boolean", "default": false },
    "synth_cpp11":       { "type": "boolean", "default": false },
    "synth_spark2014":   { "type": "boolean", "default": false },
    "simpl_cad":         { "type": "boolean", "default": false },

    "solver_z3":   { "type": "boolean", "default": false },
    "solver_cvc4": { "type": "boolean", "default": false },

    "rec_unrolling": {
      "type": "string",
      "pattern": "^(auto|[0-9]+)$",
      "description": "Depth for recursive unrolling"
    },

    "assume_unary_seq": { "type": "boolean", "default": false },
    "solver_statistics": { "type": "boolean", "default": false },

    "get_trace": { "type": "boolean", "default": false },

    "trace_style": { "type": "string" },

    "eval": { "type": "boolean", "default": false },

    "include_env": {
      "type": "string",
      "description": "Path to environment file"
    },

    "input_sexp":   { "type": "string" },
    "input_exp_dsl":    { "type": "string" },
    "input_latexeq":{ "type": "string" },
    "input_rmdsl":  { "type": "string" },

    "config_file": { "type": "string" },

    "set_monitor_period":     { "type": "integer", "minimum": 0 },
    "set_buffer_size":        { "type": "integer", "minimum": 0 },
    "set_min_inter_time":     { "type": "integer", "minimum": 0 },
    "set_max_period":         { "type": "integer", "minimum": 0 },
    "set_event_type":         { "type": "string" },
    "set_event_subtype":      { "type": "string" },
    "set_monitor_name_prefix":{ "type": "string" },
    "set_monitor_time_unit":  { "type": "string" },

    "out_file": { "type": "string" },
    "out_src":  { "type": "string" },
    "out_dir":  { "type": "string" },

    "verbose": {
      "type": "integer",
      "minimum": 0,
      "description": "Verbosity level"
    }
  },

  "additionalProperties": false,
  
  "anyOf": [
    { "required": ["input_sexp"] },
    { "required": ["input_exp_dsl"] },
    { "required": ["input_latexeq"] },
    { "required": ["input_rmdsl"] },
    { "required": ["config_file"] }
  ]
}
|}

(** Validate JSON value against schema loaded from file *)
let validate_schema_file ~schema_file json =
  match Jsonschema.validate_file ~schema:schema_file json with
  | Ok () -> Ok ()
  | Error err -> Error (Jsonschema.Validation_error.to_string err)

let validate_schema_string ~schema ~json =
  match Jsonschema.validate_strings ~schema ~json () with
  | Ok () -> Ok ()
  | Error err -> Error (Jsonschema.Validation_error.to_string err)

let read_json_from_file file_path =
  try Yojson.Basic.from_file file_path
  with Yojson.Json_error msg ->
    failwith
      (Printf.sprintf "Failed to parse JSON from %s: %s" file_path msg)
