open Z3
open Z3.Arithmetic
open Z3.Boolean
open Sequence.Snapshot

let create_z3_context () =
  let cfg = [("model", "true"); ("proof", "false")] in
  Z3.mk_context cfg

(* Function to calculate OBB corners *)
let calculate_obb element =
  let open Float in
  let cx = element.position.x in
  let cy = element.position.y in
  let yaw = element.position.yaw in
  let radius = element.region.radius in
  (* BB before the rotation *)
  let corners =
    [ (-.radius, -.radius)
    ; (radius, -.radius)
    ; (radius, radius)
    ; (-.radius, radius) ]
  in
  (* Rotate the corners then translate them*)
  let cos_yaw = cos yaw in
  let sin_yaw = sin yaw in
  List.map
    (fun (lx, ly) ->
      let rx = (lx *. cos_yaw) -. (ly *. sin_yaw) +. cx in
      let ry = (lx *. sin_yaw) +. (ly *. cos_yaw) +. cy in
      (rx, ry) )
    corners

let create_event_expressions ctx trace =
  let x = Real.mk_const_s ctx "x" in
  let y = Real.mk_const_s ctx "y" in
  List.map
    (fun event ->
      let event_id = event.event_id in
      let event_exprs_and_obbs =
        List.map
          (fun element ->
            match element.region with
            | {region_type= "circle"; radius} ->
                let element_x =
                  Real.mk_numeral_s ctx (string_of_float element.position.x)
                in
                let element_y =
                  Real.mk_numeral_s ctx (string_of_float element.position.y)
                in
                let radius =
                  Real.mk_numeral_s ctx (string_of_float radius)
                in
                let dx = Arithmetic.mk_sub ctx [x; element_x] in
                let dy = Arithmetic.mk_sub ctx [y; element_y] in
                let distance_squared =
                  Arithmetic.mk_add ctx
                    [ Arithmetic.mk_mul ctx [dx; dx]
                    ; Arithmetic.mk_mul ctx [dy; dy] ]
                in
                let radius_squared =
                  Arithmetic.mk_mul ctx [radius; radius]
                in
                (* Create the expression: (x - element_x)^2 + (y -
                   element_y)^2 < radius^2 *)
                let expr = mk_lt ctx distance_squared radius_squared in
                (* Create the OBB *)
                let obb_corners = calculate_obb element in
                ( Boolean.mk_const_s ctx (Printf.sprintf "a_%d" event_id)
                , expr
                , obb_corners )
            | {region_type= "triangle"; radius} ->
                let element_x =
                  Real.mk_numeral_s ctx (string_of_float element.position.x)
                in
                let element_y =
                  Real.mk_numeral_s ctx (string_of_float element.position.y)
                in
                let circumradius =
                  Real.mk_numeral_s ctx (string_of_float radius)
                in
                let dx = Arithmetic.mk_sub ctx [x; element_x] in
                let dy = Arithmetic.mk_sub ctx [y; element_y] in
                let distance_squared =
                  Arithmetic.mk_add ctx
                    [ Arithmetic.mk_mul ctx [dx; dx]
                    ; Arithmetic.mk_mul ctx [dy; dy] ]
                in
                let circumradius_squared =
                  Arithmetic.mk_mul ctx [circumradius; circumradius]
                in
                (* Create the expression: (x - element_x)^2 + (y -
                   element_y)^2 < circumradius^2 *)
                let expr = mk_lt ctx distance_squared circumradius_squared in
                (* Create the OBB *)
                let obb_corners = calculate_obb element in
                ( Boolean.mk_const_s ctx (Printf.sprintf "a_%d" event_id)
                , expr
                , obb_corners )
            | _ -> failwith "Unsupported region type" )
          event.elements
      in
      (* Combine all element expressions *)
      let combined_expr =
        mk_and ctx (List.map (fun (_, expr, _) -> expr) event_exprs_and_obbs)
      in
      let obbs = List.map (fun (_, _, obb) -> obb) event_exprs_and_obbs in
      ( event_id
      , combined_expr
      , List.map (fun (z3_var, _, _) -> z3_var) event_exprs_and_obbs
      , obbs ) )
    trace.trace

let () =
  let ctx = create_z3_context () in
  (* Get all .json files in the current directory *)
  let json_files =
    Sys.readdir "." |> Array.to_list
    |> List.filter (fun file -> Filename.check_suffix file ".json")
  in
  List.iter
    (fun json_file ->
      let json_path = json_file in
      Printf.printf "Processing file: %s\n" json_path ;
      let json = Yojson.Basic.from_file json_path in
      let parsed_trace = parse json in
      let event_expressions = create_event_expressions ctx parsed_trace in
      List.iter
        (fun (event_id, combined_expr, z3_vars, obbs) ->
          Printf.printf "Event ID: %d\n" event_id ;
          Printf.printf "Expression: %s\n" (Expr.to_string combined_expr) ;
          List.iter
            (fun z3_var ->
              Printf.printf "Z3 Variable: %s\n" (Expr.to_string z3_var) )
            z3_vars ;
          List.iter
            (fun obb_corners ->
              Printf.printf "OBB Corners:\n" ;
              List.iter
                (fun (x, y) -> Printf.printf "  (x=%f, y=%f)\n" x y)
                obb_corners )
            obbs )
        event_expressions ;
      Printf.printf "\n" )
    json_files
