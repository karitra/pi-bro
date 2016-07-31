

module Tiff : sig 
	val generate_brightness_vec: string -> int * int * string * float array
end = struct

	open Core.Std

	open Ctypes
	open Foreign
	open Unsigned

	let tiff_format = "rgba"

	type tif = unit ptr
	let tif : tif typ = ptr void

	let tiff_open  = foreign "TIFFOpen"  (string @-> string @-> returning tif)
	let tiff_close = foreign "TIFFClose" (tif @-> returning void)
	let tiff_get_field = foreign "TIFFGetField" (tif @-> int @-> ptr int @-> returning void)

	let raster_t = ptr uint32_t
	let raster_opt_t = ptr_opt uint32_t

	let tiff_alloc = foreign "_TIFFmalloc" (int64_t @-> returning raster_opt_t)
	let tiff_free = foreign "_TIFFfree" (raster_t @-> returning void)

	let tiff_read_img = foreign "TIFFReadRGBAImage" (tif @-> int @-> int @-> raster_t @-> int @-> returning int)


	let color_brightness_norm v =
		let open Unsigned.UInt32.Infix in
		let byte_mask = (Unsigned.UInt32.of_int 0xFF) in
			let r = (v lsr 24)  in
			let g = (v lsr 16) land byte_mask in
			let b = (v lsr  8) land byte_mask in
			let i = Unsigned.UInt32.to_int (r+r+b+g+g+g)
				in 
					(float_of_int i)
					/. 
					(6. *. 256.)

	let generate_brightness_vec fname =
		(* print_endline "Running"; *)
		let width_field_number = 256 and
			height_field_number = 257 and 
			bits_per_sample = 258 in
		let im = tiff_open fname "r" in
			let w_ptr = allocate int 0 and
				h_ptr = allocate int 0 and
				bits_ps_ptr = allocate int 0 in
					tiff_get_field im width_field_number w_ptr;
					tiff_get_field im height_field_number h_ptr;
					tiff_get_field im bits_per_sample bits_ps_ptr;
					let (w,h) = !@ w_ptr, !@ h_ptr in
					(* printf "w:%d h:%d, bits: %d\n" (!@ w_ptr) (!@ h_ptr) (!@bits_ps_ptr); *)
					let raster_size = w * h in
					let to_alloc = Int64.of_int ((sizeof uint32_t) * raster_size) in
					(* printf "to alloc: %d\n" ((sizeof uint32_t) * raster_size); *)
					let raster_opt = tiff_alloc to_alloc in
						let rast = match raster_opt with
							| Some p -> if (tiff_read_img im (!@ w_ptr) (!@ h_ptr) p 0) <> 0 then
											p
										else
											failwith (sprintf "Failed to get raster data for image: %s" fname)
							| None   -> failwith "Failed to allocate raster buffer" in
						let img_arr = CArray.from_ptr rast raster_size in
						let result  = Core.Std.Array.create ~len:(CArray.length img_arr) 0.0 in
							for i = 0 to (CArray.length img_arr) - 1 do
								result.(i) <-
									CArray.unsafe_get img_arr i
									|> color_brightness_norm
							done;
							tiff_free rast;
							tiff_close im;
							(* print_endline "done"; *)
							(w,h,tiff_format,result)
end

let _test () =
	let open Core.Std in
		for _i = 1 to 1 (* 1_000_000_000 *) do
			let (_,_,_,vec) = Tiff.generate_brightness_vec Sys.argv.(1) in 
				Array.iter ~f:(fun x -> printf "%.6f\n" x) vec
		done


include Tiff

(* let () = _test () *)

