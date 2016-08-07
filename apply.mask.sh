#!/bin/bash

MASK_SFX='.amask.png'
LABELED_SFX='.labeled.png'
#FRAME_SIZE='400x380\!'

if [ $# -lt 2 ]; then
	echo "Wrong number of parameters, should be: SRC_DIR DST_DIR"
	exit 1
fi

CPUs=`nproc --all`
echo Running on $CPUs CPUS

echo Preparing masks in folder $1

find $1 -type f -name "*_mask.tif" | xargs -P $CPUs -I '{MASK}' sh -c 'FL=$(basename {MASK}); SUBJ=${FL%%_*}; P="{MASK}"; SRC_FILE="${P%_*}.tif"; STRIP_MASK=${FL%_*}; FRAME=${STRIP_MASK##*_}; convert  -alpha Shape {MASK} -fill "rgba(0,250,0,0.5)" -opaque white '$2'/${FL%.*}'$MASK_SFX'; convert -undercolor green -fill white -annotate +10+10 "Subj: $SUBJ $FRAME" ${SRC_FILE} '$2'/$STRIP_MASK'$LABELED_SFX


echo Combine images files in folder $2

find $2 -type f -name "*$MASK_SFX" | xargs -P $CPUs -I '{MASK}' sh -c 'FL=$(basename {MASK}); IMG_BASE="${FL%_mask'$MASK_SFX'}"; LABELED="${IMG_BASE}'$LABELED_SFX'"; composite -compose Multiply '"$2"'/$LABELED {MASK} '"$2"'/${IMG_BASE}.png'

