#!/bin/bash

# mount a bee!
OUT=$(hdiutil mount [[BEE_IMAGE]])

read -r PATH VOL <<< $OUT

echo "path $VOL"