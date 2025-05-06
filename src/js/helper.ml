(* js_helper *)
open Js_of_ocaml

module Sys = struct
  let argv =
    let getenv x =
      try List.assoc x Js_of_ocaml.Url.Current.arguments
      with Not_found -> ""
    in
    let explode s = s |> String.to_seq |> List.of_seq in
    let rec console_arg_split s cm b =
      match s with
      | [] -> [cm]
      | a :: x ->
          if a = ' ' && b mod 2 = 0 then [cm] @ console_arg_split x "" b
          else
            console_arg_split x
              (cm ^ if a = '\"' then "" else String.make 1 a)
              (if a = '\"' then b + 1 else b)
    in
    Array.of_list (console_arg_split (explode (getenv "argv")) "" 0)

  let executable_name = "rmtld3synth"

  let file_exists c = false

  let remove c = ()

  let time c = 0.

  let is_directory c = false

  let command x = 1
end

let output_buffer_ = Buffer.create 1000

let flush x =
  let module J = Js_of_ocaml.Js.Unsafe in
  let () =
    J.call
      (Js_of_ocaml.Js.Unsafe.pure_js_expr "postMessage")
      (Js_of_ocaml.Js.Unsafe.pure_js_expr "self")
      [|J.inject (Js_of_ocaml.Js.string (Buffer.contents output_buffer_))|]
  in
  Buffer.clear output_buffer_

let print_string = Buffer.add_string output_buffer_

let print_char = Buffer.add_char output_buffer_

let print_newline () = print_char '\n'

let print_endline s =
  print_string (s ^ "\n") ;
  flush ()

let caml_ml_output_char = print_char

let exit s = "Error Code:" ^ (s |> string_of_int) |> print_endline

let _ =
  Js_of_ocaml.Js.Unsafe.global##.readFile
  := Js_of_ocaml.Js.Unsafe.inject Js_of_ocaml.Sys_js.read_file ;
  print_endline
    "--------------------------------------------------------------------------------\n" ;
  let date = new%js Js_of_ocaml.Js.date_now in
  let date_str = Js_of_ocaml.Js.to_string date##toString in
  print_endline date_str ;
  print_endline
    "--------------------------------------------------------------------------------\n" ;
  Array.iter print_endline Sys.argv ;
  print_endline
    "--------------------------------------------------------------------------------\n"
