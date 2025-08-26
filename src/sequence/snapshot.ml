(* This module provides functions to parse a sequence of snapshots (trace) from JSON format.
 * The trace structure consists of events, each containing elements with positions and regions.
 *)

open Yojson.Basic.Util

(* Trace Strucure*)
type position =
  {x: float; y: float; z: float; yaw: float; pitch: float; roll: float}

type region = {region_type: string; radius: float}

type element =
  {id: int; element_type: string; position: position; region: region}

type event = {event_id: int; timestamp: string; elements: element list}

type trace = {trace: event list}

(* Helpers to handle parsing*)
let to_number json =
  try to_float json
  with Yojson.Basic.Util.Type_error _ -> float_of_int (to_int json)

let to_int_or_string json =
  try to_int json
  with Yojson.Basic.Util.Type_error _ -> int_of_string (to_string json)

(* Function to parse a position *)
let parse_position json =
  { x= json |> member "x" |> to_number
  ; y= json |> member "y" |> to_number
  ; z= json |> member "z" |> to_number
  ; yaw= json |> member "yaw" |> to_number
  ; pitch= json |> member "pitch" |> to_number
  ; roll= json |> member "roll" |> to_number }

(* Function to parse region *)
let parse_region json =
  { region_type= json |> member "type" |> to_string
  ; radius= json |> member "radius" |> to_number }

(* Function to parse an element*)
let parse_element json =
  { id= json |> member "ID" |> to_int_or_string
  ; element_type= json |> member "type" |> to_string
  ; position= json |> member "position" |> parse_position
  ; region= json |> member "region" |> parse_region }

(* Function to parse an event*)
let parse_event json =
  { event_id= json |> member "eventID" |> to_int_or_string
  ; timestamp= json |> member "timestamp" |> to_string
  ; elements= json |> member "elements" |> to_list |> List.map parse_element
  }

(* Function to parse the trace *)
let parse json =
  {trace= json |> member "trace" |> to_list |> List.map parse_event}

(*Get the coordinates from the trace*)
let get_circle_params (trace: trace) car_id t ctx =
  let event = List.nth trace.trace t in
  let element = List.find (fun e -> e.id = car_id) event.elements in
  let x = Z3.Arithmetic.Real.mk_numeral_s ctx (string_of_float element.position.x) in
  let y = Z3.Arithmetic.Real.mk_numeral_s ctx (string_of_float element.position.y) in
  let r = Z3.Arithmetic.Real.mk_numeral_s ctx (string_of_float element.region.radius) in
  (x, y, r)

(*Get the geomtric expression that represents an object in the trace*)
let get_car_geometric_expressions ctx (trace: trace) (target_car_id: int) (x_var: Z3.Expr.expr) (y_var: Z3.Expr.expr) : Z3.Expr.expr list =
  List.filter_map (fun event ->
    List.find_opt (fun element -> element.id = target_car_id) event.elements
    |> Option.map (fun element ->
      match element.region with
      | { region_type = "circle"; radius } ->
          let element_x = Z3.Arithmetic.Real.mk_numeral_s ctx (string_of_float element.position.x) in
          let element_y = Z3.Arithmetic.Real.mk_numeral_s ctx (string_of_float element.position.y) in
          let r_val = Z3.Arithmetic.Real.mk_numeral_s ctx (string_of_float radius) in
          let dx = Z3.Arithmetic.mk_sub ctx [x_var; element_x] in
          let dy = Z3.Arithmetic.mk_sub ctx [y_var; element_y] in
          let distance_squared = Z3.Arithmetic.mk_add ctx [
            Z3.Arithmetic.mk_mul ctx [dx; dx];
            Z3.Arithmetic.mk_mul ctx [dy; dy]
          ] in
          let radius_squared = Z3.Arithmetic.mk_mul ctx [r_val; r_val] in
          Z3.Arithmetic.mk_lt ctx distance_squared radius_squared
      | { region_type = "triangle"; radius } ->
          let element_x = Z3.Arithmetic.Real.mk_numeral_s ctx (string_of_float element.position.x) in
          let element_y = Z3.Arithmetic.Real.mk_numeral_s ctx (string_of_float element.position.y) in
          let circumradius_val = Z3.Arithmetic.Real.mk_numeral_s ctx (string_of_float radius) in
          let dx = Z3.Arithmetic.mk_sub ctx [x_var; element_x] in
          let dy = Z3.Arithmetic.mk_sub ctx [y_var; element_y] in
          let distance_squared = Z3.Arithmetic.mk_add ctx [
            Z3.Arithmetic.mk_mul ctx [dx; dx];
            Z3.Arithmetic.mk_mul ctx [dy; dy]
          ] in
          let circumradius_squared = Z3.Arithmetic.mk_mul ctx [circumradius_val; circumradius_val] in
          Z3.Arithmetic.mk_lt ctx distance_squared circumradius_squared
      | _ -> failwith ("Unsupported region type for car " ^ string_of_int target_car_id)
    )
  ) trace.trace