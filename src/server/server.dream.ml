(** Run generation command with JSON input.
    Validates the input against the schema, applies options, and executes code generation. *)
let run_cmd fmt body =
  let job_helper = Helper.mk_helper () in
  (* default settings job_helper *)
  Options.default_settings job_helper ;
  (* validate json *)
  Json_schema.validate_schema_string ~schema:Json_schema.schema_json
    ~json:body
  |> function
  | Error msg ->
      Format.fprintf fmt "Error: Input JSON does not conform to schema." ;
      Dream.log "Schema validation failed for input: %s" body ;
      failwith ("Schema validation failed: " ^ msg)
  | Ok () ->
      (* set options from json *)
      let json = Yojson.Safe.from_string body in
      ( try
          Options.apply_options_from_assoc_list
            (json |> Yojson.Safe.Util.to_assoc)
            job_helper
        with Failure msg ->
          Format.fprintf fmt "Error: %s" (String.escaped msg) ;
          Dream.log "apply_options_from_assoc_list failed: %s" msg ;
          failwith ("Failed to apply options.") ) ;
      Dream.log "Settings after applying JSON options: %s"
        (Helper.get_json_string_of_settings job_helper) ;
      (* Check if version flag is set to display version info, otherwise
         proceed with generation *)
      if Helper.get_setting_bool "version" job_helper then
        Format.fprintf fmt "Version %s" Version.git
      else (
        (* Store version in settings for use during code generation *)
        Helper.set_setting "version" (Helper.Txt Version.git) job_helper ;
        (* run generation based on options *)
        if Options.ocaml_lang job_helper then
          let module Conv_ocaml =
            Synthesis.Standard.Translate (Synthesis.Ocaml) in
          Synthesis.Ocaml.synth_ocaml fmt Conv_ocaml.synth job_helper
        else if Options.cpp11_lang job_helper then (
          let module Conv_cpp11 =
            Synthesis.Standard.Translate (Synthesis.Cpp11) in
          Options.default_cpp11_settings job_helper ;
          Synthesis.Cpp11.synth_cpp11 fmt Conv_cpp11.synth job_helper )
        else if Options.spark2014_lang job_helper then
          let module Conv_spark2014 =
            Synthesis.Standard.Translate (Synthesis.Spark2014) in
          Synthesis.Spark2014.synth_spark2014 fmt Conv_spark2014.synth
            job_helper
        else if Options.smtlibv2_lang job_helper then
          let module Smtlib = Synthesis.Standard.Translate (Synthesis.Smtlib2) in
          let lst =
            Synthesis.Smtlib2.synth_smtlib fmt Smtlib.synth job_helper
          in
          (* convert string list to multipart message *)
          Helper.to_multipart_message fmt lst
        else
          Format.fprintf fmt "Error: No valid generation language specified."
        )

(** API endpoint to retrieve JSON schema. *)
let schema () =
  [ Dream.get "/api/schema" (fun _request ->
        Dream.json Json_schema.schema_json ) ]

(** API endpoint to retrieve current settings as JSON. *)
let status () =
  [ Dream.get "/api/settings" (fun _request ->
        let settings = Helper.get_json_string_of_settings Options.helper in
        Dream.json settings ) ]

(** API endpoint to retrieve available command-line options and their descriptions. *)
let options () =
  [ Dream.get "/api/options" (fun _request ->
        let options = Options.speclist in
        let options_json =
          List.map
            (fun (key, spec, doc) ->
              let command_name = String.(sub key 2 (length key - 2)) in
              let description =
                match String.split_on_char '\n' doc with
                | [] -> ""
                | first :: _ -> first
              in
              `Assoc
                [ ( "command"
                  , `String
                      (String.map
                         (fun c -> if c = '-' then '_' else c)
                         command_name ) )
                ; ( "values"
                  , `String
                      ( match spec with
                      | Arg.Int _ -> "integer"
                      | Arg.String _ -> "string"
                      | Arg.Unit _ -> "boolean"
                      | _ -> "other" ) )
                ; ("description", `String (String.trim description)) ] )
            options
        in
        Dream.json (Yojson.Basic.to_string (`List options_json)) ) ]

open Lwt.Infix

(** Hash table storing currently pending requests: request_id -> (timestamp, target) *)
let pending_requests = Hashtbl.create 100

(** Hash table storing completed requests: hash_id -> (status, result, timestamp) *)
let completed_requests = Hashtbl.create 100

(** Hash table storing cancellable request tasks: hash_id -> Lwt.t *)
let cancellable_requests = Hashtbl.create 100

(** Clean up completed requests older than one day. *)
let cleanup_old_requests () =
  let now = Unix.gettimeofday () in
  let one_day = 86400.0 in
  (* seconds in a day *)
  Hashtbl.filter_map_inplace
    (fun _hash_id (status, result, timestamp) ->
      if now -. timestamp > one_day then None
      else Some (status, result, timestamp) )
    completed_requests

(** Background task that runs cleanup every hour. *)
let rec cleanup_loop () =
  Lwt_unix.sleep 3600.0 (* Run every hour *)
  >>= fun () ->
  Dream.log "Running cleanup of old requests" ;
  cleanup_old_requests () ;
  Dream.log "Cleanup completed" ;
  cleanup_loop ()

let () = Lwt.async cleanup_loop

(** Middleware to track request lifecycle: start, completion, and cleanup. *)
let request_tracking handler request =
  let timestamp = Unix.gettimeofday () in
  let target = Dream.target request in
  let request_id = target ^ "_" ^ string_of_float timestamp in
  Hashtbl.add pending_requests request_id (timestamp, target) ;
  let active_count = Hashtbl.length pending_requests in
  Dream.log "Request %s started: %s (total active: %d)" request_id target
    active_count ;
  Lwt.finalize
    (fun () -> handler request)
    (fun () ->
      Hashtbl.remove pending_requests request_id ;
      let remaining = Hashtbl.length pending_requests in
      Dream.log "Request %s completed: %s (remaining: %d)" request_id target
        remaining ;
      Lwt.return_unit )

(** Convert Unix time to ISO 8601 format string. *)
let iso8601 t =
  let open Unix in
  let t = gmtime t in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" (t.tm_year + 1900)
    (t.tm_mon + 1) t.tm_mday t.tm_hour t.tm_min t.tm_sec

(** API endpoints for request control: submit, query, and cancel requests. *)
let request_control () =
  [ Dream.get "/api/requests/active" (fun _request ->
        let active_count = Hashtbl.length pending_requests in
        Dream.json (Printf.sprintf {|{"active_requests":%d}|} active_count) )
  ; Dream.get "/api/requests/pending" (fun _request ->
        let pending =
          Hashtbl.fold
            (fun id (timestamp, target) acc ->
              `Assoc
                [ ("id", `String id)
                ; ("timestamp", `Float timestamp)
                ; ("target", `String target) ]
              :: acc )
            pending_requests []
        in
        Dream.json (Yojson.Basic.to_string (`List pending)) )
  ; Dream.get "/api/request/:hash_id" (fun request ->
        let hash_id = Dream.param request "hash_id" in
        match Hashtbl.find_opt completed_requests hash_id with
        | Some (status, result, timestamp) ->
            let json =
              `Assoc
                [ ("status", `String status)
                ; ("result", `String result)
                ; ("timestamp", `String (iso8601 timestamp)) ]
            in
            Dream.json (Yojson.Safe.to_string json)
        | None ->
            Dream.json
              (Printf.sprintf {|{"status":"not_found","result":""}|}) )
  ; Dream.delete "/api/request/:hash_id" (fun request ->
        let hash_id = Dream.param request "hash_id" in
        match Hashtbl.find_opt cancellable_requests hash_id with
        | Some task ->
            Lwt.cancel task ;
            Hashtbl.remove cancellable_requests hash_id ;
            let timestamp = Unix.gettimeofday () in
            Hashtbl.replace completed_requests hash_id
              ("cancelled", "", timestamp) ;
            Dream.json
              (Printf.sprintf {|{"status":"cancelled","hash_id":"%s"}|}
                 hash_id )
        | None ->
            Dream.json
              (Printf.sprintf {|{"status":"not_found","hash_id":"%s"}|}
                 hash_id ) )
  ; Dream.post "/api/request" (fun request ->
        Dream.body request
        >>= fun body ->
        let hash_id =
          Digest.to_hex
            (Digest.string (body ^ string_of_float (Unix.gettimeofday ())))
        in
        let timestamp = Unix.gettimeofday () in
        Hashtbl.add completed_requests hash_id ("pending", "", timestamp) ;
        let task =
          request_tracking
            (fun _req ->
              (let buf = Buffer.create 1024 in
               let fmt = Format.formatter_of_buffer buf in
               try
                 run_cmd fmt body ;
                 Format.pp_print_flush fmt () ;
                 let result = Buffer.contents buf in
                 (*Lwt_unix.sleep 60.0 >>= fun () ->*)
                 Hashtbl.replace completed_requests hash_id
                   ("completed", result, timestamp)
               with e ->
                 Format.pp_print_flush fmt () ;
                 let error_msg = Buffer.contents buf in
                 let backtrace = Printexc.get_backtrace () in
                 Dream.log "Error in request %s: %s\n%s" hash_id
                   (Printexc.to_string e) backtrace ;
                 Hashtbl.replace completed_requests hash_id
                   ("error", error_msg, timestamp) ) ;
              Hashtbl.remove cancellable_requests hash_id ;
              Lwt.return_unit )
            request
        in
        Hashtbl.add cancellable_requests hash_id task ;
        Lwt.async (fun () -> task) ;
        Dream.json
          (Printf.sprintf {|{"hash_id":"%s","status":"pending"}|} hash_id) )
  ]

(** Main entry point: sets up routing and starts the Dream web server. *)
let () =
  let app =
    Dream.router
      (List.concat
         [ [ Dream.get "/favicon.ico"
               (Dream.from_filesystem "static" "favicon.ico")
           ; Dream.get "/" (Dream.from_filesystem "static" "index.html")
           ; Dream.get "/style.css"
               (Dream.from_filesystem "static" "style.css")
           ; Dream.get "/logo.png"
               (Dream.from_filesystem "static" "logo.png") ]
         ; [ Dream.get "/js/:file" (fun request ->
                 Dream.from_filesystem "static/js"
                   (Dream.param request "file")
                   request )
           ; Dream.get "/bundles/:file" (fun request ->
                 Dream.from_filesystem "static/bundles"
                   (Dream.param request "file")
                   request ) ]
         ; options () @ schema () @ request_control () ] )
    |> Dream.logger
  in
  Dream.run ~interface:"0.0.0.0" ~port:8001 app
