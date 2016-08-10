#!/bin/bash

CPUs=`nproc --all`
find $1 -name "*.vecs.hist.path" | xargs -P $CPUs -I "{PATHS_FILE}" ./gen_gif_anim.native {PATHS_FILE}  $2 $3 -delay 250
