

open Str


let read_line file =
	let file_data = "" in
	try
	    let file_data = input_line file in
	    file_data

  	with End_of_file ->
	    "file_data"

let rec read_file file lines =
	let line = read_line file in
	if line = "file_data" then lines else read_file file (lines^"\n"^line)

let _ =
	
	print_endline ("Welcome to gtool, a graphic generator for rtemlib!");

	(* let define three files to be parsed *)
	(* x86 ocaml output *)
	let x86_ocaml_filename = "results_ocaml.txt" in
	(* x86 c++ output *)
	let x86_cpp_filename = "results_cpp.txt" in
	(* arm c++ output *)
	let arm_cpp_filename = "results_arm.txt" in

	let bins = [10; 100; 1000] in

	for i=1 to 4 do
	List.fold_left (fun _ filename ->

		let output = ref "" in

		for trace_idx=1 to 3 do
		let stream = open_in filename in
		let log_file = read_file stream "" in
		(* expression to be parsed *)
		let re = Str.regexp ( "__unit_test_monitor_set1_c"^(string_of_int i)^"_[0-9](): TIME_MES: \\([0-9]+\\):\\([0-9]+\\)\n") in

		let rec get_groups file init_p l =
		try
			let dd = search_forward re file init_p in
			get_groups file (dd+1) ((Str.matched_group 1 file, Str.matched_group 2 file)::l)
		with Not_found ->
			l
		in

		let x = get_groups log_file 1 [] in
		
		output := !output^List.fold_left
		( fun a (t,id) ->
			let id = int_of_string id in
			if
			(0 < id && id <= 10 && trace_idx = 1) ||
			(10 < id && id <= 100 && trace_idx = 2) ||
			(100 < id && id <= 1000 && trace_idx = 3) then
			"("^string_of_int (trace_idx)^", "^t^")"^" "^a else a
		) "" x;

		close_in stream;
		done;

		output := "\\addplot coordinates{"^ !output;
		output := !output^"};";
		Printf.printf "%s\n" !output;

	) () [x86_ocaml_filename; x86_cpp_filename; arm_cpp_filename];

	done;
(*
	(* lets load files *)
	let stream = open_in x86_ocaml_filename in

	let log_file = read_file stream "" in

	(*print_endline log_file;*)

	let re = Str.regexp "DURATION_TIME:\\([0-9]+\\):\\([0-9]+\\)\n" in

	(*let lines = Str.split re log_file in*)

	(*List.fold_left (fun a b -> (print_endline (b^"\n"))) () lines;*)

	let rec get_groups file init_p l =
		try
		let dd = search_forward re file init_p in
		get_groups file (dd+1) ((Str.matched_group 1 file, Str.matched_group 2 file)::l)
		with Not_found ->
			l
	in

	let x = get_groups log_file 1 [] in

	let out = "\\addplot[boxplot] table[row sep=\\\\,y index=0] {" in
	let out = out^List.fold_left ( fun a (t,id) -> (t^" "^id)^"\\\\"^a ) "" x in
	let out = out^"};" in

	let stream = open_out ("out.tikz") in
	Printf.fprintf stream "%s\n" out;
	close_out stream;
*)
	(*print_endline (string_of_bool x);*)

	(*print_endline (ss^" "^ss2);*)
