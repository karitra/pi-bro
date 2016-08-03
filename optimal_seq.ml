
module Adj_matrix : sig

	module LD = Lacaml.D
	open LD

	type t = float array array

	val make : LD.vec array -> t
	val total_half : t -> float
	val length : t -> int
	val row_as_array : t -> int -> float array

	val dump : t -> t
end = struct
	(* open Core *)
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

	let row_as_array t i = t.(i)

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
	(* open Core *)
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

	type ptype
	(* type pset *)
	val solve : ?take_first:int -> ?path_pool_limit:int -> int -> bool -> Adj_matrix.t -> ptype list
	val print_set : ptype list -> unit

end = struct
	(* open Core *)
	open Core.Std
	open Core_extended
	open Core_bench

	open Adj_matrix

	type ptype = float * int * int * int list [@@deriving sexp]
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
		let selected = Hash_set.create () ~hashable:Int.hashable ~size in
		(* TODO: make vector based version in order to avoid float unboxing and run benchmarks *)
		let _select_next from =
			let sl = List.range 0 size
				|> List.filter ~f:(fun i -> (Hash_set.mem selected i) = false && i <> from) 
				|> List.map ~f:(fun dest -> dest, 1.0 /. mat.(from).(dest)) in
				let total = List.fold ~init:0.0 ~f:(fun acc (_, d) -> acc +. d ) sl in
					assert (total > 0.0);
					let sl_norm = List.map ~f:(fun (i,d) -> 
							(* printf "%d => %d : %.3f\n" from i (1.0 /. d /. total); *)
							i, d /. total) sl and
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
								| None -> 
										(* then select first available, but probably an error rounding here *)
										match sl_norm with
										| (i,_)::_ -> i
										| [] -> 
											(* we must have some node in a list *)
											raise (Internal_error 
												(sprintf "failed to find destination node, sel_prob: %.6f comul_prob: %.6f"
													sel_prob
													!comul_prob))
							in
								Hash_set.add selected new_selected; 
								new_selected
		in
		(* Bench shows that array version at least twice faster in huge arrays e.g. >= ~200 elements *)
		let _select_next_arr from =
			let row = Adj_matrix.row_as_array mat from in
			let ign_bits = Bitarray.create (Array.length row) in
			let sa = Array.mapi 
						~f:(fun i el -> 
							if Hash_set.mem selected i = false && i <> from then
								1.0 /. el
							else
								(Bitarray.set ign_bits i true; 0.0) ) row
			in
				(* printf "From = %d\nHashtbl.length %d\n" from (Hash_selected); *)
				(* Array.iter ~f:(fun el -> printf "  %f\n" el) sa; *)
				let total = Array.sum (module Float) ~f:ident sa in
					(* printf "Sum of arr sized %d is %f\n" (Array.length sa) total; *)
					assert (total > 0.0);
					Array.iteri ~f:(fun i el -> sa.(i) <- el /. total) sa;
					let sel_prob = Random.float 1.0 in
					let comul_prob = ref 0.0 in
					let new_selected =
						match Array.findi ~f:(fun i el_prob ->
								comul_prob := !comul_prob +. el_prob;
								sel_prob <= !comul_prob && Bitarray.get ign_bits i = false
							) sa with
						| Some (i,_) -> 
							(* printf "selected: %d\n" i; *)
							i
						| None -> (* get first possible *)
							match Array.findi ~f:(fun i _ -> Bitarray.get ign_bits i = false) sa with
							| Some (i,_) -> i
							| None -> (* we must have some node allowable to be selected in the array *)
								raise (Internal_error 
									(sprintf 
										"failed to find destination node, sel_prob: %.6f comul_prob: %.6f"
										sel_prob !comul_prob)) 
					in
						Hash_set.add selected new_selected;
						new_selected
		in
		let sel_init = Random.int size in
		begin
			Hash_set.add selected sel_init;
			let first = ref sel_init in
			let last = ref 0 in
			let rec loop acc sel size = 
				if size = 0 then (last := sel; sel::acc)
				else loop (sel::acc) (_select_next_arr sel) (size-1)
			in
				(!first, !last, loop [] sel_init (size-1))
		end


	module Paths_Set = Set.Make(
		struct
			type t = ptype [@@deriving sexp]
			let compare (k1,_,_,_) (k2,_,_,_) = Float.compare k1 k2
		end
	)

	type pset = Paths_Set.t

	let solve ?(take_first=7) ?(path_pool_limit=1000) gen_lim add_noise (mat:Adj_matrix.t) = 
		
		assert (Adj_matrix.length mat >= 0);
		assert (gen_lim >= 0);

		let jitter_divs = [|10713.0; 50117.0; 30571.0; 70903.0; 317953.0|] in
		let jitter v =
			let esp = v /. jitter_divs.(Random.int (Array.length jitter_divs)) in
				if Random.bool () then
					v +. esp
				else
					v -. esp
		in

		let disort = if add_noise then jitter else ident in
		let endpoints_hist = Int.Table.create ~size:(Adj_matrix.length mat) () in

		let rec loop acc_set lim =
			if lim = 0 then
				acc_set
			else 

			begin		

				let (first, last, path) = generate_path mat in
				let path_cost = disort (path_cost path mat) in

				let update =
					function
					| Some i -> i + 1
					| None -> 1 in
					let pset_size = Paths_Set.length acc_set in
					if pset_size > path_pool_limit then
						(* printf "Pruned length %d\n" (Paths_Set.length pruned_set); *)
						let () = 
							match Paths_Set.min_elt acc_set with
							| Some (_,f,l,_) -> 
									Hashtbl.update endpoints_hist f ~f:update;
									Hashtbl.update endpoints_hist l ~f:update
							| None -> () in
								(* with removing maximum *)
								loop (Paths_Set.remove_index acc_set (pset_size-1)) (lim-1)
					else
						loop (Paths_Set.add acc_set (path_cost, first, last, path)) (lim-1)
			end

			in
			let dump_endpoint_hist oc =
				fprintf oc "%s\n" "Endpoints histogram:";
				let cmp = Tuple2.compare ~cmp1:(fun _ _ -> 0) ~cmp2:(fun cnt1 cnt2 -> cnt2 - cnt1) in
				Hashtbl.to_alist endpoints_hist
				|> List.sort ~cmp
				|> List.iter ~f:(fun (nd, cnt) -> fprintf oc "  %d => %d\n" nd cnt)
			
			in
			begin
				loop (Paths_Set.empty) gen_lim 
				|> (fun set -> 
					dump_endpoint_hist stdout;
					set)
				|> Paths_Set.to_list 
				|> (fun li -> List.take li take_first)
			end


	let print_set =
		let print (cost,_,_,li) = 
			printf "path cost: %.3f nodes: %s\n" cost 
				(List.fold ~init:"" ~f:(fun acc el -> acc ^ (string_of_int el) ^ " ") li) 
		in
			List.iter ~f:print 

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
