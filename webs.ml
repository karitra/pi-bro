
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
open Printf


(* Global constants *)
let bf_size = 16 * 1024
let els_per_page = 25

let gen_html_img_list rpath fls =
	let bf = Buffer.create bf_size in
	let bf_app = Buffer.add_string bf in
	let gen_html_img_row fl =
		begin
			bf_app "<tr><td>";
			bf_app fl;
			bf_app "</td></tr>"
		end in
			begin
				print_endline (sprintf "Generating page for: %s" rpath);
				bf_app "<html><head><title>Images thumbnails</title></head><body>";
				bf_app (Printf.sprintf "<h3>Listing for path %s</h3>" rpath);
				bf_app "<table>";
				Array.iter ~f:gen_html_img_row fls;
				bf_app "</table>";
				bf_app "</table></body></html>";
				print_endline (Printf.sprintf "Generating done: %s" rpath);
				Buffer.contents bf
			end

let gen_thumb_page rpath off =
	printf "GET %s\n" rpath;
	let path = Sys.getcwd () ^ "/" ^ rpath in
	let fls =
		let rg = Regex.create_exn "^(\\d+)_(\\d+)\\.tiff$" in
		let matcher = Regex.matches rg in match Sys.is_directory path with
			| `No | `Unknown  -> print_endline (Printf.sprintf "path not a dir: %s" path); Array.create 0 ""
			| `Yes -> 
				print_endline (Printf.sprintf "path is a dir: %s" path);
				let filtered_tiff_array =
					path
					|> Sys.ls_dir 
					|> List.filter ~f:matcher 
					|> List.to_array in
					let from_idx = off and 
						to_idx   = off + els_per_page and
						arr_len  = Array.length filtered_tiff_array in
					eprintf "Array size: %d, offset: %d, upto: %d\n" arr_len from_idx to_idx;
					Array.slice filtered_tiff_array from_idx to_idx
			in
			gen_html_img_list rpath fls

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
	let callback conn req body = 
		let path = req |> Request.uri |> Uri.path in
			match path with
				| "/favivon.ico" -> 
					Server.respond_string ~status:`Not_found ~body:(html_not_found path) ()
    			| _ -> body 
    				|> Cohttp_lwt_body.to_string 
    				(* >|= (fun body -> html_not_found path) *)
    				>|= (fun body ->
    					try 
    						(gen_thumb_page path 0) 
    					with 
    					| x -> html_exn x path)
    				>>= (fun body -> Server.respond_string ~status:`OK ~body ()) in 
    					Server.create ~mode:(`TCP (`Port 8088)) (Server.make ~callback ())

(* Entry point *)
let _ = ignore (Lwt_main.run srv)
