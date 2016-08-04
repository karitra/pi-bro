open Core.Std
open Core_extended.Std

let read_path_seq fname = 
	let f ic =
		match In_channel.input_line ic with
		| Some ln ->
			(* printf "path string: %s\n" ln; *)
			let l = match String.rindex ln ':' with
				| Some i -> String.drop_prefix ln (i+1)
				| None -> "" in
					String.strip l
					|> String.split ~on:' '
					|> List.map ~f:int_of_string
		| None -> []
	in
	In_channel.with_file fname ~f

let extract_subject fname = 
	String.split ~on:'.' (Filename.basename fname)
	|> List.hd
	|> (function
		| Some v -> (* printf "val [%s]\n" v; *) int_of_string v
		| None -> failwith "Incorrect path file naming, should be <NUM>.<extension>"
	)

let compose_cmd subj folder path_list delay output =
	(* Don't know in advance real size, but should assume multiply of pages*)
	let default_buffer_size = 32 * 4 * 1024 in
	let (gif_w, gif_h) = 400, 320 in
	let b = Buffer.create default_buffer_size in
	let add_buff = 
		Buffer.add_string b 
	in
	let file_pfx = sprintf "%d_" subj in
	let page_def = sprintf "-resize %dx%d\\!" gif_w gif_h in
	let label_pfx = "-annotate +20+20 " in
		(* add_buff (sprintf "-size %dx%d " gif_w gif_h); *)
		add_buff (sprintf "-delay %dx1000 " delay);
		add_buff "-font Helvetiva -fill green -pointsize 16 ";
		List.iter ~f:(fun i -> 
			let i_plus = i + 1 in
			add_buff (sprintf "%s %s \"subj: %d\" %s/%s%d.tif " 
				page_def label_pfx subj folder file_pfx i_plus) ) path_list;
		add_buff "-loop 0 ";
		(* add_buff (sprintf "%s/%s " folder output); *)
		add_buff output;
		Buffer.contents b

let () =
	let run path_file output delay dump () =
		let subj = extract_subject path_file and
			path = read_path_seq path_file and
			folder = Filename.dirname path_file
		in
		let c = compose_cmd subj folder path delay output in
			if dump then
				printf "CMD:\n\t%s\nlen: %d\n" c (String.length c)
			else 
				();
			Shell.sh "convert %s" c
	in
	let cmd = 
		Command.basic
			~summary:"Generate gif animation (using external command from IM package) for best path"
			Command.Spec.(
				empty
				+> anon ("path_file" %:string)
				+> anon ("output_gif" %:string)
				+> flag "-delay" (optional_with_default 120 int) ~doc:"ms inter frame delay"
				+> flag "-dump" (no_arg) ~doc:" dump 'convert' command"
			)
			run
	in
	Command.run ~version:"1.0" cmd
