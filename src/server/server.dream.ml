let run_cmd fmt body =
  let module Conv_ocaml = Synthesis.Standard.Translate (Synthesis.Ocaml) in
  Options.default_settings Options.helper ;
  (* validate json *)
  Json_schema.validate_schema_string
    ~schema:Json_schema.schema_json ~json:body
  |> function
  | Error msg ->
      Format.fprintf fmt "Error: Input JSON does not conform to schema." ;
      Dream.log "Schema validation failed for input: %s" body ;
      failwith ("Schema validation failed: " ^ msg)
  | Ok () ->
      (* set options from json *)
      let json = Yojson.Safe.from_string body in
      Options.apply_options_from_assoc_list
        (json |> Yojson.Safe.Util.to_assoc) ;
      Dream.log "Settings after applying JSON options: %s"
        (Helper.get_json_string_of_settings Options.helper) ;
      Helper.set_setting "version" (Helper.Txt "") Options.helper ;
      (* run synthesis based on options *)
      if Options.ocaml_lang () then
        Synthesis.Ocaml.synth_ocaml fmt Conv_ocaml.synth Options.helper

let status () =
  [ Dream.get "/api/settings" (fun _request ->
        let settings = Helper.get_json_string_of_settings Options.helper in
        Dream.json settings ) ]

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
                [ ("command", `String command_name)
                ; ( "values"
                  , `String
                      ( match spec with
                      | Arg.Int _ -> "integer"
                      | Arg.String _ -> "string"
                      | Arg.Unit _ -> "unit"
                      | _ -> "other" ) )
                ; ("description", `String description) ] )
            options
        in
        Dream.json (Yojson.Basic.to_string (`List options_json)) ) ]

open Lwt.Infix

let pending_requests =
  Hashtbl.create 100 (* request_id -> (timestamp, target) *)

let completed_requests =
  Hashtbl.create 100 (* hash_id -> (status, result, timestamp) *)

let cancellable_requests = Hashtbl.create 100 (* hash_id -> Lwt.t *)

(* Clean up completed requests older than one day *)
let cleanup_old_requests () =
  let now = Unix.gettimeofday () in
  let one_day = 86400.0 in
  (* seconds in a day *)
  Hashtbl.filter_map_inplace
    (fun _hash_id (status, result, timestamp) ->
      if now -. timestamp > one_day then None
      else Some (status, result, timestamp) )
    completed_requests

(* Background cleanup task *)
let rec cleanup_loop () =
  Lwt_unix.sleep 3600.0 (* Run every hour *)
  >>= fun () ->
  Dream.log "Running cleanup of old requests" ;
  cleanup_old_requests () ;
  Dream.log "Cleanup completed" ;
  cleanup_loop ()

let () = Lwt.async cleanup_loop

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
        | Some (status, result, _timestamp) ->
            Dream.json
              (Printf.sprintf {|{"status":"%s","result":"%s"}|} status
                 (String.escaped result) )
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
         ; options () @ status () @ request_control () ] )
    |> Dream.logger
  in
  Dream.run ~interface:"0.0.0.0" ~port:8001 app
