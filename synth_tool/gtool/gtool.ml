

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

	(* lets load the file *)
	let stream = open_in "../monitor_set1/results.txt" in

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

	(*print_endline (string_of_bool x);*)

	(*print_endline (ss^" "^ss2);*)
