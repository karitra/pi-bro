#/bin/bash
#
echo Building web service...
corebuild -j 4 -use-ocamlfind -tag thread -pkgs cohttp.lwt,base64,re2,lablgtk2,core_extended webs.native

echo Building vectors generator...
corebuild -j 4 -use-ocamlfind -tag thread -pkgs camlimages.tiff,re2 gen_vec.native
# corebuild -j 4 -use-ocamlfind -tag thread -pkgs camlimages.tiff,re2 gen_vec.byte

echo Building MC solver...
corebuild -j 4 -use-ocamlfind -tag thread -pkgs lacaml optimal_seq.native
