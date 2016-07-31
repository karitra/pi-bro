#/bin/bash
#
echo Building web service...
corebuild -j 4 -use-ocamlfind -tag thread -pkgs cohttp.lwt,base64,re2,lablgtk2,core_extended webs.native

#
# TODO: remove camlimages from packages
#
echo Building vectors generator...
corebuild -j 4 -cflags -unsafe -use-ocamlfind -tag thread -pkgs ctypes.foreign,camlimages.tiff,re2 gen_vec.native
# corebuild -j 4 -use-ocamlfind -tag thread -pkgs camlimages.tiff,re2 gen_vec.byte

echo Building MC solver...
corebuild -j 4 -cflags -unsafe -use-ocamlfind -tag thread -pkgs lacaml optimal_seq.native

# echo Building image loader tests...
# corebuild -j 3 -use-ocamlfind -tag thread -pkgs ctypes.foreign -lflags -cclib,-ltiff  vec_of_tiff.native
