

module Adj_matrix = sig

	module LD = Lacaml.D
	open LD

	type t
	val make : LD.vec array -> t
	val total_half : t -> float

end = struct
	open Core
	open Core.Std

	type t = float array array

	module Pair = struct
		module T = struct
			type t = int * int
			let compare : compare
			let hash : Hashtbl.hash 
		end
		include T
		include Hashtbl.Make(T)
	end


	let make vecs =
		let n = Array.length vecs in
		let table = Pair.Table.create ~size:n in
		let rec fill i = 
			if i = n then
				()
			else
				for j = i + 1 to n - 1
					let diff = sub vecs.(i) vecs.(j) |> sqr_nrm in
						Hashtbl.set table ~key:(i,j) ~data:diff
				end
				fill (i+1);
		in
			fill 0;
			let res_mat = Array.make_matrix n n 0.0 in
				Hashtbl.iter (fun (i,j) data -> res_mat.(i).(j) <- res_mat.(j).(i) <- data) table;
				res_mat

	let total_half t =
		let s = Array.fold ~init:0.0 ~f:(fun acc el -> acc .+ (Array.sum el) ) t in
			s ./ 2.0

end

module VectorReader sig =
	module LD = Lacaml.D
	open LD

	val read_as_array : string -> vec array
end = struct
	open Core
	open Core.Std

	let read_and_proc_lines_as_list ic = 
		let parse li ln =
			match String.index ln ':' with
			| Some i -> 
				(
					String.drop_suffix ln i 
					|> String.split ~on:' '
					|> List.map ~f:Float.of_string 
					|> LD.Vec.of_list
				) :: li
			| None -> li
		in
		In_channel.fold_lines ic ~init:[]  ~f:parse |> List.rev

	let read_and_proc_lines_as_array ic =
		read_and_proc_lines_as_list ic |> Array.of_list
		
	let read_as_array name =
		In_channel.with_file name ~f:read_and_proc_lines_as_array
end


module StochasticSolver = sig

end = struct
	open Core
	open Core.Std
	open Adj_matrix

	let make_probability_matrix mat = 
		let n = Array.length mat in
		let pm = Array.make_matrix n n 0.0 in 
		let n_1 = n - 1 in
			for i = 0 to n_1 do
				let sum = Array.sum mat.(i) in
				begin
					assert sum > 0;
					for j = 0 to n_1 do
						pm.(i).(j) <- 1.0 .- mat.(i).(j) ./ sum
					done
				end
			done;
			pm

	let path_cost p mat = 
		let rec calc acc = function
			| [] | [_] -> acc 
			| hd1::hd2::tl -> calc (acc .+ mat.(hd1).(hd2)) (hd2::tl)
		in
		calc 0.0 p

	let generate_path mat = 
		let size = Array.length mat in
		let sel_init = Random.int size in
		let selected = Hash_set.create ~hashable:Int.hashable ~size in
		let select_next from = 
			let sl = List.range 0 size 
				|> List.filter ~f:(fun i -> (Hash_set.mem selected i) = false || i = from) 
				|> List.map ~f:(fun dest -> dest, mat.(from).(dest)) in
				let total = List.fold ~init:0.0 ~f:(fun acc (_, d) -> acc .+ d ) sl in
					assert (total > 0.0);
					let sl_norm = List.map ~f:(fun _ (i,d) -> i, 1 .- d ./ total) sl and
						sel_prob = Random.float 1.0 in
						let comul_prob = ref 0.0 in
						let new_selected = 
							List.find 
								~f:(fun (_, el_prob) -> comul_prob := !comul_prob .+ el_prob; sel_prob >= comul_prob)
								sl_norm |> fst in
							begin
								Hash_set.add selected new_selected;
								new_selected
							end
		in
			Hash_set.add selected sel_init;
			let rec loop acc sel size = 
				if size = 0 then
					acc
				else
					loop (sel::acc) (select_next sel) (size - 1)
			in
				loop [] sel_init (size - 1)


	let solve mat gen_lim = 
		let n = Array.length mat in
		assert (lim >= 0);
		let pmat = make_probability_matrix mat in
		let rec loop acc lim =
			if lim = 0 then
				acc
			else
				let path = generate_path pmat and
					path_cost = path_cost path
				in
				loop ((path_cost, path) :: acc) (lim - 1)

end
