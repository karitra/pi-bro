open Core_extended.Std
open Core.Std

let read_path_seq fname = 
	let f ic =
		let pattern = "nodes:" in
		let rec input_loop () =
			match In_channel.input_line ic with
			| Some ln -> 
					begin
						match String.substr_index ln ~pattern with
						| Some i ->
							String.drop_prefix ln (i + (String.length pattern))
							|> String.strip
							|> String.split ~on:' '
							|> List.map ~f:int_of_string
						| None -> input_loop ()
					end
			| None -> []
		in
			input_loop ()
	in
	In_channel.with_file fname ~f

let extract_subject fname = 
	String.split ~on:'.' (Filename.basename fname)
	|> List.hd
	|> (function
		| Some v -> (* printf "val [%s]\n" v; *) int_of_string v
		| None -> failwith "Incorrect path file naming, should be <NUM>.<extension>" )

let compose_cmd subj folder path_list delay output =
	(* Don't know in advance real size, but should assume multiply of pages*)
	let default_buffer_size = 32 * 4 * 1024 in
	let (gif_w, gif_h) = 400, 320 in
	let b = Buffer.create default_buffer_size in
	let add_buff = 
		Buffer.add_string b 
	in
	let file_pfx = sprintf "%d_" subj in
		(* add_buff (sprintf "-size %dx%d " gif_w gif_h); *)
		List.iter ~f:(fun i ->
			let i_plus = i + 1 in
			add_buff (sprintf "%s/%s%d.png " 
				folder file_pfx i_plus) ) path_list;
		add_buff "-loop 0 ";
		add_buff (sprintf "-delay %dx1000 " delay);
		add_buff (sprintf "-resize %dx%d " gif_w gif_h);
		add_buff output;
		Buffer.contents b

let () =
	let run path_file folder output delay dump () =
		let subj = extract_subject path_file and
			path = read_path_seq path_file 
			(* and *)
			(* folder = Filename.dirname path_file *)
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
				+> anon ("images_folder" %:string)
				+> anon ("output_gif" %:string)
				+> flag "-delay" (optional_with_default 120 int) ~doc:"ms inter frame delay"
				+> flag "-dump" (no_arg) ~doc:" dump 'convert' command"
			)
			run
	in
	Command.run ~version:"1.1" cmd
