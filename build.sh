#/bin/bash
#
corebuild -j 4 -use-ocamlfind -tag thread -pkgs cohttp.lwt,base64,re2,lablgtk2,core_extended webs.native
