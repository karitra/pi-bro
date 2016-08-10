#!/bin/bash

CPUs=`nproc --all`

find $1 -name "*.vecs.hist.path" | xargs -P $CPUs -I "{PATHS_FILE}" sh -c 'FL=$(basename {PATHS_FILE}); SUBJ=${FL%%_*}; ../gen_gif_anim.native {PATHS_FILE} '$2' '$3'/anim.${SUBJ}.gif -delay 230'
