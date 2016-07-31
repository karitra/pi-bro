module Adj_matrix : sig

	module LD = Lacaml.D
	open LD

	type t = float array array

	val make : LD.vec array -> t
	val total_half : t -> float
	val length : t -> int

	val dump : t -> t
end = struct
	open Core
	open Core.Std

	module LD = Lacaml.D
	open LD

	type t = float array array

	module Pair = struct
		
		module T = struct
			type t = Int.t * Int.t [@@deriving sexp]
			let compare = compare
			let hash = Hashtbl.hash
		end

		include T
		include Hashable.Make(T)
	end


	let make vecs =
		let n = Array.length vecs in
		let table = Pair.Table.create () ~size:n in
		let rec fill i = 
			if i >= n then
				()
			else begin
				for j = i + 1 to n - 1 do
					let diff = Vec.sub vecs.(i) vecs.(j) in 
					let norm = Vec.sqr_nrm2 diff in
						Hashtbl.set table ~key:(i,j) ~data:norm
				done;
				fill (i+1)
			end
		in
			fill 0;
			let res_mat = Array.make_matrix ~dimx:n ~dimy:n 0.0 in
				begin
					Hashtbl.iteri 
						~f:(fun ~key:(i,j) ~data -> res_mat.(i).(j) <- data; res_mat.(j).(i) <- data) table;
					res_mat
				end

	let length t = Array.length t

	let total_half t =
		let s = Array.fold ~init:0.0 ~f:(fun acc el -> acc +. (Array.sum (module Float) el ~f:ident) ) t in
			s /. 2.0

	let dump t =
		let print_row r = 
			Array.iteri 
				~f:(fun i x -> print_float x; print_char (if i < Array.length r - 1 then ' ' else '\n')) 
				r
			in (Array.iter ~f:print_row t; t)
end

module VectorReader : sig
	module LD = Lacaml.D
	open LD

	val read_as_array : string -> vec array
end = struct
	open Core
	open Core.Std

	module LD = Lacaml.D
	open LD

	let read_and_proc_lines_as_list ic = 
		let parse li ln =
			match String.index ln ':' with
			| Some i -> 
				(
					(* eprintf "target line is [%s], index is %d\n" ln i; *)
					String.drop_prefix ln (i+1)
					|> String.strip
					|> String.split ~on:' '
					|> List.map ~f:Float.of_string 
					|> Vec.of_list
				) :: li
			| None -> li
		in
		In_channel.fold_lines ic ~init:[]  ~f:parse |> List.rev

	let read_and_proc_lines_as_array ic =
		read_and_proc_lines_as_list ic |> Array.of_list
		
	let read_as_array name =
		In_channel.with_file name ~f:read_and_proc_lines_as_array
end


module StochasticSolver : sig

	exception Internal_error of string
	open Adj_matrix

	type pset
	val solve : ?take_first:int -> int -> bool -> Adj_matrix.t -> pset 
	val print_set : pset -> unit

end = struct
	open Core
	open Core.Std
	open Adj_matrix

	type ptype = float * int list [@@deriving sexp]
	exception Internal_error of string

	let _make_probability_matrix mat = 
		let n = Array.length mat in
		let pm = Array.make_matrix ~dimx:n ~dimy:n 0.0 in 
		let n_1 = n - 1 in
		begin
			for i = 0 to n_1 do
				let sum = Array.sum (module Float) mat.(i) ~f:ident in
				begin
					assert (sum > 0.0);
					for j = 0 to n_1 do
						pm.(i).(j) <- 1.0 -. mat.(i).(j) /. sum
					done
				end
			done;
			pm
		end

	let path_cost p mat = 
		let rec calc acc = function
			| [] | [_] -> acc 
			| hd1::hd2::tl -> calc (acc +. mat.(hd1).(hd2)) (hd2::tl)
		in
		calc 0.0 p

	let generate_path (mat:Adj_matrix.t) = 
		let size = Adj_matrix.length mat in
		let sel_init = Random.int size in
		let selected = Hash_set.create () ~hashable:Int.hashable ~size in
		let select_next from = 
			let sl = List.range 0 size
				|> List.filter ~f:(fun i -> (Hash_set.mem selected i) = false && i <> from) 
				|> List.map ~f:(fun dest -> dest, mat.(from).(dest)) in
				let total = List.fold ~init:0.0 ~f:(fun acc (_, d) -> acc +. 1.0 /. d ) sl in
					assert (total > 0.0);
					let sl_norm = List.map ~f:(fun (i,d) -> 
							(* printf "%d => %d : %.3f\n" from i (1.0 /. d /. total); *)
							i, 1.0 /. d /. total) sl and
						sel_prob = Random.float 1.0 in
						let comul_prob = ref 0.0 in
						let new_selected = 
							match List.find
								~f:(fun (_, el_prob) -> 
									comul_prob := !comul_prob +. el_prob;
									(* printf "dice: %.3f comul: %.3f\n" sel_prob !comul_prob; *)
									sel_prob <= !comul_prob)
								sl_norm with
								| Some (i,_) -> 
									(* printf "selected: %d\n" i; *)
									i
								| None -> raise (Internal_error "failed to find destination node!")
							in
								begin
									Hash_set.add selected new_selected;
									new_selected
								end
		in
		begin
			Hash_set.add selected sel_init;
			let rec loop acc sel size = 
					if size = 0 then sel::acc
					else loop (sel::acc) (select_next sel) (size-1)
				in
					loop [] sel_init (size-1);
		end


	module Paths_Set = Set.Make(
		struct
			type t = ptype [@@deriving sexp]
			let compare (k1,_) (k2,_) = Float.compare k1 k2
		end
	)

	type pset = Paths_Set.t

	let solve ?(take_first=12) gen_lim add_noise (mat:Adj_matrix.t) = 
		
		assert (Adj_matrix.length mat >= 0);
		assert (gen_lim >= 0);

		let sentry = ref 0 in
		let acc_limited_set acc elt =
			incr sentry;
			(* printf "sentry %d\n" !sentry; *)
			let s = Paths_Set.add acc elt in
				if !sentry < take_first then `Continue s else `Stop s
		in
		let jitter_divs = [|10713.0; 50117.0; 30571.0; 70903.0; 317953.0|] in
		let jitter v =
			let esp = v /. jitter_divs.(Random.int (Array.length jitter_divs)) in
				if Random.bool () then
					v +. esp
				else
					v -. esp
		in
		let disort = if add_noise then jitter else ident in
		let rec loop acc_set lim =
			if lim = 0 then
				acc_set
			else 
			begin		
				let path = generate_path mat in
				let path_cost = disort (path_cost path mat) in
				if Paths_Set.length acc_set >= take_first then
					begin
						sentry := 0;
						let pruned_set = Paths_Set.fold_until ~init:Paths_Set.empty ~f:acc_limited_set acc_set
						in
							(* printf "Pruned length %d\n" (Paths_Set.length pruned_set); *)
							loop (Paths_Set.add pruned_set (path_cost, path)) (lim-1)
					end
				else
					loop (Paths_Set.add acc_set (path_cost, path)) (lim-1)
			end
			in begin
				sentry := 0;
				loop (Paths_Set.empty) gen_lim 
				|> (fun set -> sentry := 0; set)
				|> Paths_Set.fold_until ~init:Paths_Set.empty ~f:acc_limited_set
			end

	let print_set =
		let print (cost, li) = 
			printf "path cost: %.3f nodes: %s\n" cost 
				(List.fold ~init:"" ~f:(fun acc el -> acc ^ (string_of_int el) ^ " ") li) 
		in
			Paths_Set.iter ~f:print 

end

open Core.Std
open Gc

let cmd =
	let run vecs_filename repeat noise () =
		VectorReader.read_as_array vecs_filename 
		|> Adj_matrix.make 
		(* |> Adj_matrix.dump *)
		|> StochasticSolver.solve repeat noise
		|> StochasticSolver.print_set
	in
	let open Command.Spec in
		Command.basic
			~summary:"\n Run MC simulation for finding frames sequence with smallest interframe distance cost"
			Command.Spec.(
				empty
				+> flag "-file" (required string)  ~doc:"filename file with frames vectors"
				+> flag "-rolls" (required int)  ~doc:"num quantity of MC simulations"
				+> flag "-noise" (no_arg)  ~doc:"add noise to cost function output"
			)
			run

let () =
	Gc.set { ( Gc.get () ) with Gc.Control.minor_heap_size = 256 * 1024 * 16};
	Random.self_init ();
	Command.run ~version:"1.0a" cmd
