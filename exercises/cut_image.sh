#!/usr/bin/env bash
SRC=$1
FULL=${SRC%.*}_full.png
DST=${SRC%.*}_%d.png
DST0=${SRC%.*}_0.png
DST1=${SRC%.*}_1.png
DST2=${SRC%.*}_2.png
DST3=${SRC%.*}_3.png

convert $SRC -rotate 180 +repage $FULL
width=$(identify -format "%w" $FULL)> /dev/null
height=$(identify -format "%h" $FULL)> /dev/null
./grid -s $(($width/4)),$(($height/4)) $FULL $FULL

#convert $SRC -rotate 180 -crop 50%x50% +repage $DST
#width=$(identify -format "%w" $DST0)> /dev/null
#height=$(identify -format "%h" $DST0)> /dev/null


#./grid -s $(($width/2)),$(($height/2)) $DST0 $DST0
#./grid -s $(($width/2)),$(($height/2)) $DST1 $DST1
#./grid -s $(($width/2)),$(($height/2)) $DST2 $DST2
#./grid -s $(($width/2)),$(($height/2)) $DST3 $DST3
