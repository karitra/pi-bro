
open Core
open Core.Std
(* open Re2 *)

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
	let path = Sys.getcwd () ^ "/" ^ rpath in
	let bf = Buffer.create bf_size in
	let bf_app = Buffer.add_string bf in
	let fls =
		let rg = Regex.create_exn "^(\\d+)_(\\d+)\\.tiff$" in
		let matcher = Regex.matches rg in
			path |> Sys.ls_dir |> List.filter ~f:matcher
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
				(* List.iter ~f:(fun x -> bf_app x) fls; *)
				List.iter ~f:gen_html_in_bf fls;
				bf_app "</table>";
				bf_app "</table></body></html>";
				Buffer.contents bf
			end

let srv =
	let callback conn req body = 
		let url  = req |> Request.uri in
		let path = url |> Uri.path in
		(* let uri  = url |> Uri.to_string in
    	let meth    = req |> Request.meth    |> Code.string_of_method in
    	let headers = req |> Request.headers |> Header.to_string      in
    	let files = Sys.readdir path in *)
    		body |> Cohttp_lwt_body.to_string 
    			>|= (fun body -> gen_thumb_page path 0)
    			>>= (fun body -> Server.respond_string ~status:`OK ~body ()) in 
    					Server.create ~mode:(`TCP (`Port 8088)) (Server.make ~callback ())

(* Entry point *)
let _ = ignore (Lwt_main.run srv)
