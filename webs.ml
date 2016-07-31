
open Core
open Core.Std
open Core_extended.Std.Cache
open Re2

module Regex = Re2.Regex

open Sys

open Lwt
open Cohttp
open Cohttp_lwt_unix

open B64
(* open Images
open OImages
 *)
open Printf

open GdkPixbuf


module Array_view : sig
	type 'a t

	(** TODO: make interface consistent on passing 'view' type *)
	val create : 'a array -> int -> int -> 'a t
	val make_piped : int -> int -> 'a array -> 'a t

	val get : 'a t -> int -> 'a
	val length : 'a t -> int

	val fold : init:'b -> f:('b -> 'a -> 'b) -> 'a t -> 'b
	val map : f:('a -> 'b) -> 'a t -> 'b array
	val iter : f:('a -> unit) -> 'a t -> unit
end =
struct

	type 'a t = { arr: 'a array; off:int; length:int }

	let create arr off len = 
		let arr_size = Array.length arr in
			if off >= arr_size then 
				{ arr; off = 0; length = 0}
			else begin 
				if off + len > arr_size then
					{ arr; off; length = arr_size - off }
				else 
					{ arr; off; length = len}
			end

	let make_piped off len arr = create arr off len

	let get slice i = slice.arr.(slice.off + i)
	let length slice = slice.length

	let fold ~init ~f slice = 
		let rec fold_iter acc i =
			if i >= slice.off + slice.length then acc else fold_iter (f acc slice.arr.(i)) (i + 1) in
				fold_iter init slice.off 

	let map ~f slice = Array.init slice.length ~f:(fun i -> f slice.arr.(i))

	let iter ~f slice =
		ignore (fold ~init:0 ~f:(fun i _ -> f slice.arr.(slice.off + i); i + 1) slice)

end

(* Global constants *)
let bf_size = 16 * 1024
let els_per_page = 24
let thumb_width = 320
let thumb_height = 200
let img_bf_size = thumb_width * thumb_height * 2

let get_binary_data file =
(* 	printf "trying to load image file: %s\n" file; *)
	let buf = Buffer.create img_bf_size in
	let pix = GdkPixbuf.from_file_at_size file ~width:thumb_width ~height:thumb_height in
		begin
			GdkPixbuf.save_to_buffer pix ~typ:"png" buf;
			Buffer.contents buf;
		end

let get_binary_data_masked file file_mask =
	(* printf "trying to load image file with mask: %s\n" file; *)
	let buf  = Buffer.create img_bf_size in
	let pix  = GdkPixbuf.from_file_at_size file      ~width:thumb_width ~height:thumb_height in
	let mask = GdkPixbuf.from_file_at_size file_mask ~width:thumb_width ~height:thumb_height in
		begin
			GdkPixbuf.composite ~dest:pix ~alpha:70 ~scale_x:1.0 ~scale_y:1.0 ~interp:`BILINEAR mask;
			GdkPixbuf.save_to_buffer pix ~typ:"png" buf;
			Buffer.contents buf;
		end
		

let gen_html_img_list fullpath rpath off fls_view =
	let bf = Buffer.create bf_size in
	let bf_app = Buffer.add_string bf in
	let gen_thumb_of_file ?file_mask file =
		match file_mask with
			| None -> B64.encode (get_binary_data file)
			| Some fm -> B64.encode (get_binary_data_masked file fm)
	in
	let render_html_file ?file_mask file =
		try
			let open Core_filename in 
				match file_mask with 
				| None ->  bf_app (gen_thumb_of_file (concat fullpath file))
				| Some fm -> 
					let (img, mask) = (concat fullpath file), (concat fullpath fm) in 
						bf_app (gen_thumb_of_file ~file_mask:mask img)
		with 
			e -> printf "Exn: %s\n" (Exn.to_string e)
	in
	let gen_html_img_row fl =
		let fl_mask = 
			let fl_prfx = String.split fl ~on:'.' |> List.hd_exn in
				(* TODO: support for tif_f_ extension *)
				fl_prfx ^ "_mask.tif" 
		in
		begin
			bf_app "<tr>";
			bf_app "<td>";
				bf_app (sprintf "<a href=\"image?name=%s&back_off=%d\">" fl off);
				bf_app fl;
				bf_app "</a></td>";
			bf_app "<td>";
			bf_app (sprintf "<img alt=\"%s\" src=\"data:image/tiff;base64," fl);
			render_html_file fl;
			bf_app "\">";
			bf_app "</td>";
			(* bf_app "<td>";
			bf_app (sprintf "<img alt=\"%s\" src=\"data:image/tiff;base64," fl_mask);
			render_html_file fl_mask;
			bf_app "\">";
			bf_app "</td>"; *)
			bf_app "<td>";
			bf_app (sprintf "<img alt=\"%s\" src=\"data:image/tiff;base64," fl_mask);
			render_html_file ~file_mask:fl_mask fl;
			bf_app "\">";
			bf_app "</td>";
			bf_app "</tr>";
		end in
			begin
				print_endline (sprintf "Generating page for: %s" fullpath);
				print_endline (sprintf "request path: %s" rpath);
				bf_app "<html><head><title>Images thumbnails</title></head><body>";
				bf_app (Printf.sprintf "<h3>Listing for path %s</h3>" fullpath);
				bf_app "<table border=\"1\">";
				Array_view.iter ~f:gen_html_img_row fls_view;
				bf_app "</table>";
				bf_app "</table></body></html>";
				bf_app (sprintf "<span><a href=\"?offset=%d\">next page</a></span>" (off + els_per_page));
				print_endline (Printf.sprintf "Generating done: %s" fullpath);
				Buffer.contents bf
			end

let create_img_matcher pattern = 
	let rg = Regex.create_exn pattern in Regex.matches rg

let cached_dir_content ls_dir_f sort =
	let tb = Hashtbl.Poly.create () in
		(fun path ->
			if Hashtbl.length tb > 32 then Hashtbl.clear tb;
			match Hashtbl.find tb path with
			| Some data -> data
			| None ->
				let content = ls_dir_f path |> sort in
					ignore (Hashtbl.add tb ~key:path ~data:content);
				content)

let cached_dir_content_lru ls_dir_f sort =
	let tb = Lru.create ~destruct:None 16 in
		(fun path ->
			match Lru.find tb path with
			| Some data -> data
			| None ->
				let content = ls_dir_f path |> sort in
					ignore (Lru.add tb ~key:path ~data:content);
				content)


let gen_thumb_page rpath off mem_f =
	printf "GET %s @ %d\n" rpath off;
	let matcher = create_img_matcher "^(\\d+)_(\\d+)\\.tiff?$" in
	let open Core_filename in
		let path = concat (Sys.getcwd ()) rpath in
		match Sys.is_directory path with
			| `No | `Unknown  -> ""
			| `Yes ->
				print_endline (Printf.sprintf "path is a dir: %s" path);
					path
					|> mem_f
					|> Array.filter ~f:matcher
					|> Array_view.make_piped off els_per_page
					|> gen_html_img_list path rpath off

let html_not_found path =
	sprintf "<html><body>\
           <h2>Not Found</h2><p><b>%s</b>was not found on this server</p>\
           <hr />\
           </body></html>" path

let html_exn ex = sprintf
			"<html>\
				<body>\
					<h2>Exception \"%s\" @ path %s</h2>\
				</body>\
			</html>" (Exn.to_string ex)

let srv =
	let mem_readdir = cached_dir_content_lru Sys.readdir (Array.sorted_copy ~cmp:String.compare) in
	let callback _ req body =
		let uri = req |> Request.uri in
		let path =  uri |> Uri.path in
			let path_li = String.drop_prefix path 1 |> String.split ~on:'/' in
			let real_path = match List.tl path_li with
							| Some tl -> String.concat ~sep:"/" tl
							| None -> "/"
			in match List.hd path_li |> Option.value ~default:"view" with
				| "view" | "" ->
					let offset = match List.Assoc.find (Uri.query uri) "offset" with
						| Some off_li -> 
							if List.is_empty off_li then 
								0 
							else (try
								List.hd off_li |> Option.value ~default:"0" |> Int.of_string
							with Failure _ -> 0)
						| None -> 0
					in
    				let request_handler _ = gen_thumb_page real_path offset mem_readdir in
    					body
    						|> Cohttp_lwt_body.to_string 
    						>|= request_handler
    						>>= (fun body -> Server.respond_string ~status:`OK ~body ())
    			| "image" ->
    				let request_handler _ = gen_thumb_page real_path 0 mem_readdir in
    					body
    						|> Cohttp_lwt_body.to_string 
    						>|= request_handler
    						>>= (fun body -> Server.respond_string ~status:`OK ~body ())
				| _ -> Server.respond_string ~status:`Not_found ~body:(html_not_found path) ()
			in
						Server.create ~mode:(`TCP (`Port 8088)) (Server.make ~callback ())

(* Entry point *)
let _ = ignore (Lwt_main.run srv)
