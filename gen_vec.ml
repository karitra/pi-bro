open Images
open Tiff

open Core.Std
open Re2.Std

let generate_vectors = 
	List.map ~f:Vec_of_tiff.generate_brightness_vec 

(* TODO: remove *)
let _generate_vectors_deprecated img_files_li = 
	let gray_colors = float(256) in
	let get_rgb_brightness  c = float(Color.brightness c) /. gray_colors in
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
		if Sys.is_directory ~follow_symlinks:true folder = `Yes then
			let re = Re2.of_string "(\\d+)_(\\d+)\\.tif" in
			let open Option in
			let open Re2.Infix in
			let match_subj_and_frame str = 
				match Re2.find_submatches re str with
				| Ok v -> 
				 	assert (Array.length v >= 3);
				 	begin match Option.both v.(1) v.(2) with
					 	| Some (subj_str, frame_str) -> 
					 		Some (
					 			int_of_string subj_str, 
					 			int_of_string frame_str)
					 	| None -> None
				 	end
				| Error err -> failwith (Error.to_string_hum err)
			in
				Sys.ls_dir folder
				|> List.filter ~f:(fun fl -> fl =~ re)
				|> List.iter   ~f:(fun fl ->
					match match_subj_and_frame fl with 
					| Some (subj, frame) -> 
						let data = match Hashtbl.find table subj with
							| Some d -> Set.add d frame
							| None ->   Int.Set.singleton frame 
						in
							ignore (Hashtbl.set table ~key:subj ~data)
					| None -> () );
				table
		else
			table

let make_subj_files_list folder subj frames_set =
	let make_fname =
		sprintf "%s/%d_%d.tif" folder subj
	in
	let f acc el =
		(make_fname el)::acc
	in
		Set.fold ~init:[] ~f frames_set |> List.rev

let cmd = 
	let run folder_in folder_out () =
		gen_subjects_lists folder_in
			|> Hashtbl.to_alist
			|> List.map ~f:(fun (subj, frames) -> (subj, (make_subj_files_list folder_in subj frames)) )
			|> List.iter ~f:(fun (subj, frames_files) -> 
								generate_vectors frames_files
								|> store_vectors (sprintf "%s/%d.vecs" folder_out subj) )
	in
	let open Command.Spec in
		Command.basic
			~summary:"\n Generate vector files of images in specified folder"
			Command.Spec.(
				empty
				+> anon ("input_folder" %: string)
				+> anon ("output_folder" %: string)
			)
			run

let () =
	Command.run ~version:"1.0" cmd
