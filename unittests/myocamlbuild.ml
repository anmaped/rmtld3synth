(* Open the ocamlbuild world... *)
open Ocamlbuild_plugin

open Ocamlbuild_pack

(* We work with commands so often... *)
open Command

let ocamlfind_query pkg =
  let cmd = Printf.sprintf "ocamlfind query %s" (Filename.quote pkg) in
  My_unix.run_and_open cmd (fun ic ->
      Log.dprintf 5 "Getting Ocaml directory from command %s" cmd;
      input_line ic
    )

(* This dispatch call allows to control the execution order of your
   directives. *)
let () =
  dispatch begin function
    (* Add our rules after the standard ones. *)
  | After_rules ->

                  Pathname.define_context "." ["."; "gtests"];

  let sexplib_dir = ocamlfind_query "sexplib" in
  let pa_sexp_conv_dir = ocamlfind_query "pa_sexp_conv" in
  let type_conv_dir = ocamlfind_query "type_conv" in
  (*let batteries_dir = ocamlfind_query "batteries" in*)

  (*ocaml_lib ~extern:true ~dir:sexplib_dir "sexplib";
  ocaml_lib ~extern:true ~dir:batteries_dir "batteries";
  ocaml_lib ~extern:true ~dir:type_conv_dir "type_conv"; *)

  flag ["ocaml"; "pp"; "use_sexplib.syntax"]
  & S[
     A"-I"; A type_conv_dir;
     A"-I"; A sexplib_dir;
     A"-I"; A pa_sexp_conv_dir;
     A"pa_type_conv.cma";
     A"pa_sexp_conv.cma"
     ]

  | _ -> ()
  end
