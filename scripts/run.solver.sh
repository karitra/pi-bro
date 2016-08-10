#!/bin/bash

CPUs=`nproc --all`
#CPUs=1
ROLLS="1_200_000"

find $1 -name '*.vecs' | xargs -P "$CPUs" -I '{SUBJ}' sh -c "./optimal_seq.native -file {SUBJ} -rolls $ROLLS > {SUBJ}.path"

