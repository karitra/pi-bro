open Images
open Tiff

open Core.Std
open Re2.Std


let generate_vectors img_files_li = 
	let gray_colors = float(256) in
	let get_rgb_brightness c = float(Color.brightness c) /. gray_colors in
	let get_rgba_brightness c = float(Color.brightness c.color) /. gray_colors in
	let get_gray_brightness c = float(c) /. gray_colors in
	let combine getter brightness x y =
		let c = getter x y in
			brightness c
	in
	let f fname =
		printf "Making vector from image: [%s]\n" fname;
		let img = Images.load fname [] in
		let w,h = Images.size img in
		let format = ref "na" in
		let getter =
			match img with
			| Index8 im  -> format := "index8"  ; combine (Index8.get im) get_gray_brightness
			| Index16 im -> format := "index16" ; combine (Index16.get im) get_gray_brightness
			| Rgb24 im   -> format := "rgb"     ; combine (Rgb24.get im) get_rgb_brightness
			| Rgba32 im  -> format := "rgba"    ; combine (Rgba32.get im) get_rgba_brightness
			| _ -> failwith (sprintf "unsupported image format for file: %s" fname) 
		in
		let arr = Array.init (w * h) ~f:(fun i -> getter (i % w) (i / w) )
		in
			(w,h,!format,arr)
	in 
		List.map ~f img_files_li


let store_vectors file vectors =
	let f ch =
		List.iter
			~f:(fun (w,h,fmt,arr) ->
				fprintf ch "%d,%d %s: " w h fmt;
				Array.iter ~f:(fun v -> fprintf ch "%3.6f " v) arr;
				Out_channel.newline ch;
			)
			vectors
	in
		Out_channel.with_file file ~f


let gen_subjects_lists folder =
	let size = 1024 * 250 in
	let table = Int.Table.create () ~size in
		if Sys.is_directory folder = `Yes then
			let re = Re2.of_string "(\\d+)_(\\d+)\\.tif" in
			let open Option in
			let open Re2.Infix in
			let int_of_re id str = 
				Re2.find_first ~sub:(`Index id) re str
					|> Result.ok 
					>>| int_of_string
				in
					Sys.ls_dir folder
					|> List.filter ~f:(fun fl -> fl =~ re)
					|> List.iter ~f:(fun fl -> 
						match Option.both (int_of_re 1 fl) (int_of_re 2 fl) with 
						| Some (subj, frame) -> 
							let data = match Hashtbl.find table subj with
								| Some d -> Int.Set.add d frame
								| None -> Int.Set.singleton frame 
							in
								ignore (Hashtbl.add table ~key:subj ~data)
						| None -> () );
					table
		else 
			table

let make_subj_files_list subj frames_set =
	let make_fname =
		sprintf "%d_%d.tif" subj
	in
	let f acc el =
		(make_fname el)::acc
	in
		Set.fold ~init:[] ~f frames_set |> List.rev

let cmd = 
	let run folder () =
		gen_subjects_lists folder
			|> Hashtbl.to_alist
			|> List.map ~f:(fun (subj, frames) -> (subj, (make_subj_files_list subj frames)) )
			|> List.iter ~f:(fun (subj, frames_files) -> 
								generate_vectors frames_files
								|> store_vectors (sprintf "%d.vecs" subj)
							)
	in
	let open Command.Spec in
		Command.basic
			~summary:"\n Generate vector files of images in specified folder"
			Command.Spec.(
				empty
				+> anon ("folder" %: string)
			)
			run

let () =
	Command.run ~version:"1.0" cmd
