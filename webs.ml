
open Core
open Core.Std
open Re2

module Regex = Re2.Regex

open Sys

open Lwt
open Cohttp
open Cohttp_lwt_unix

open B64
open Images


(* Global constants *)
let bf_size = 16 * 1024
let els_per_page = 25

let gen_thumb_page rpath off =
	Printf.printf "GET %s\n" rpath;
	let path = Sys.getcwd () ^ "/" ^ rpath in
	let bf = Buffer.create bf_size in
	let bf_app = Buffer.add_string bf in
	let fls =
		let rg = Regex.create_exn "^(\\d+)_(\\d+)\\.tiff$" in
		let matcher = Regex.matches rg in match Sys.is_directory path with
			| `No | `Unknown  -> print_endline "path not a dir"; Array.create 0 ""
			| `Yes -> let filtered_tiff_array =
						path
						|> Sys.ls_dir 
						|> List.filter ~f:matcher 
						|> List.to_array in 
						Array.slice filtered_tiff_array off (off + els_per_page)
	in
	let gen_html_in_bf fl =
		begin
			bf_app "<tr><td>";
			bf_app fl;
			bf_app "</td></tr>"
		end in
			begin
				bf_app "<html><head><title>Images thumbnails</title></head><body>";
				bf_app (Printf.sprintf "<h3>Listing for path %s</h3>" rpath);
				bf_app "<table>";
				Array.iter ~f:gen_html_in_bf fls;
				bf_app "</table>";
				bf_app "</table></body></html>";
				Buffer.contents bf
			end

let srv =
	let callback conn req body = 
		let url  = req |> Request.uri in
		print_endline ("path: " ^ (url |> Uri.path));
		let path = url |> Uri.path in
    		body |> Cohttp_lwt_body.to_string 
    			>|= (fun body -> gen_thumb_page path 0)
    			>>= (fun body -> Server.respond_string ~status:`OK ~body ()) in 
    					Server.create ~mode:(`TCP (`Port 8088)) (Server.make ~callback ())

(* Entry point *)
let _ = ignore (Lwt_main.run srv)
