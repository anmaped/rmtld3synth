open Sequence.Snapshot

(*To strings*)
let string_of_position pos =
  Printf.sprintf
    "Position(x=%.2f, y=%.2f, z=%.2f, yaw=%.2f, pitch=%.2f, roll=%.2f)" pos.x
    pos.y pos.z pos.yaw pos.pitch pos.roll

let string_of_region reg =
  Printf.sprintf "Region(type=%s, radius=%.2f)" reg.region_type reg.radius

let string_of_element elem =
  Printf.sprintf "Element(id=%d, type=%s, %s, %s)" elem.id elem.element_type
    (string_of_position elem.position)
    (string_of_region elem.region)

let string_of_event ev =
  let elements_str =
    String.concat "; " (List.map string_of_element ev.elements)
  in
  Printf.sprintf "Event(id=%d, timestamp=%s, elements=[%s])" ev.event_id
    ev.timestamp elements_str

let string_of_trace tr =
  let events_str = String.concat "\n" (List.map string_of_event tr.trace) in
  Printf.sprintf "Trace:\n%s" events_str

(* Main function to read the JSON file, parse it, and print the trace *)
let () =
  let json_file = "scenario1.json" in
  let json = Yojson.Basic.from_file json_file in
  let parsed_trace = parse json in
  Printf.printf "Parsed trace successfully!\n" ;
  Printf.printf "%s\n" (string_of_trace parsed_trace)
