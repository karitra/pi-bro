#/bin/bash
#
ocamlbuild -use-ocamlfind -tag thread -pkgs cohttp.lwt,base64,camlimages,core,re2 webs.native
