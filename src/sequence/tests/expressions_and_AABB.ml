open Z3
open Z3.Arithmetic
open Sequence.Snapshot

let create_z3_context () =
  let cfg = [("model", "true"); ("proof", "false")] in
  Z3.mk_context cfg

let create_event_expressions ctx trace =
  let x = Real.mk_const_s ctx "x" in
  let y = Real.mk_const_s ctx "y" in
  List.map
    (fun event ->
      let event_id = event.event_id in
      let per_element_details =
        List.map
          (fun element ->
            let element_id_val = element.id in
            let z3_var_name =
              Printf.sprintf "car%d_event%d" element_id_val event_id
            in
            let z3_car_var = Boolean.mk_const_s ctx z3_var_name in
            let car_expr, car_aabb =
              match element.region with
              | {region_type= "circle"; radius} ->
                  let element_x =
                    Real.mk_numeral_s ctx
                      (string_of_float element.position.x)
                  in
                  let element_y =
                    Real.mk_numeral_s ctx
                      (string_of_float element.position.y)
                  in
                  let r_val =
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
                    Arithmetic.mk_mul ctx [r_val; r_val]
                  in
                  let expr = mk_lt ctx distance_squared radius_squared in
                  let min_x = element.position.x -. radius in
                  let max_x = element.position.x +. radius in
                  let min_y = element.position.y -. radius in
                  let max_y = element.position.y +. radius in
                  let aabb = (min_x, max_x, min_y, max_y) in
                  (expr, aabb)
              | { region_type= "triangle"
                ; radius (* This is the circumradius *) } ->
                  let element_x =
                    Real.mk_numeral_s ctx
                      (string_of_float element.position.x)
                  in
                  let element_y =
                    Real.mk_numeral_s ctx
                      (string_of_float element.position.y)
                  in
                  let circumradius_val =
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
                    Arithmetic.mk_mul ctx [circumradius_val; circumradius_val]
                  in
                  let expr =
                    mk_lt ctx distance_squared circumradius_squared
                  in
                  let min_x = element.position.x -. radius in
                  let max_x = element.position.x +. radius in
                  let min_y = element.position.y -. radius in
                  let max_y = element.position.y +. radius in
                  let aabb = (min_x, max_x, min_y, max_y) in
                  (expr, aabb)
              | _ -> failwith "Unsupported region type"
            in
            (element_id_val, z3_car_var, car_expr, car_aabb) )
          event.elements
      in
      (event_id, per_element_details) )
    trace.trace

let () =
  let ctx = create_z3_context () in
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
      let event_data_list = create_event_expressions ctx parsed_trace in
      List.iter
        (fun (event_id, per_element_details) ->
          Printf.printf "Event ID: %d\n" event_id ;
          List.iter
            (fun (car_id, z3_car_var, car_expr, car_aabb) ->
              Printf.printf "  Car ID: %d\n" car_id ;
              Printf.printf "  Z3 Variable: %s\n" (Expr.to_string z3_car_var) ;
              Printf.printf "  Expression: %s\n" (Expr.to_string car_expr) ;
              let min_x, max_x, min_y, max_y = car_aabb in
              Printf.printf
                "  AABB: min_x=%f, max_x=%f, min_y=%f, max_y=%f\n" min_x
                max_x min_y max_y )
            per_element_details )
        event_data_list ;
      Printf.printf "\n" )
    json_files
