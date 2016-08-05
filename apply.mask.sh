#!/bin/bash

ASK_SFX='.amask.png'
FRAME_SIZE='400x380\!'

if [ $# -lt 2 ]; then
	echo "Wrong number of parameters, should be: SRC_DIR DST_DIR"
	exit 1
fi

CPUs=`nproc --all`
echo Running on $CPUs CPUS

echo Preparing masks in folder $1

find $1 -type f -name "*_mask.tif" | xargs -P $CPUs -I '{MASK}' sh -c 'FL=$(basename {MASK}) convert  -alpha Shape {MASK} -fill green -opaque white '$2'/${FL##*.}'$MASK_SFX

echo Preparing image files in folder $2

find $2 -type f -name "*.$MASK_SFX" | xargs -P $CPUs -I '{MASK}' sh -c 'FL=$(basename {MASK}); IMG_BASE=${FL%_mask'$MASK_SFX'}; composite -compose Multiply '$1'/${IMG_BASE}.tif {MASK} -resize '"$FRAME_SIZE $2"'/$IMG_BASE.png'


