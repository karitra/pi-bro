open Lacaml
open Core.Std
open Re2.Std

open Core_bench

let t1_split fl () = In_channel.with_file fl ~f:(fun ic -> In_channel.input_all ic)
	|> String.strip
	|> String.split ~on:' ' 
	|> List.map ~f:float_of_string
	|> Lacaml.D.Vec.of_list;;

let t1_split_arr fl () = In_channel.with_file fl ~f:(fun ic -> In_channel.input_all ic)
	|> String.strip ~drop:Char.is_whitespace
	|> String.split ~on:' ' 
	|> List.map ~f:float_of_string
	|> Array.of_list
	|> Lacaml.D.Vec.of_array;;

let t2_split_arr fl () = In_channel.with_file fl ~f:(fun ic -> In_channel.input_all ic)
	|> String.strip ~drop:Char.is_whitespace
	|> String.split ~on:' ' 
	|> Array.of_list
	|> Array.map ~f:float_of_string
	|> Lacaml.D.Vec.of_array;;

let t2_re fl () =	
	let re = Re2.of_string "([.0-9]+)" in 
	In_channel.with_file fl  ~f:(fun ic -> In_channel.input_all ic)
		|> Re2.find_all re 
		|> Result.ok 
		|> (fun x -> match x with | Some li -> li | None -> [] )  
		|> List.map ~f:float_of_string
		|> Lacaml.D.Vec.of_list;;

let make_bench name t fl =
	Bench.Test.create ~name (t fl)

let () =
	Gc.set { ( Gc.get () ) with Gc.Control.minor_heap_size = 256 * 1024 * 16};
	let fl = "tests/bench.1" in
	Command.run (Bench.make_command [
		make_bench "split" t1_split fl; 
		make_bench "split arr 1" t1_split_arr fl; 
		make_bench "split arr 2" t2_split_arr fl;
		make_bench "re" t2_re fl
	])