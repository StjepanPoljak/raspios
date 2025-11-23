#!/bin/bash

FILE_SIZE=$(stat -c %s ${3})

((SECTOR_COUNT = FILE_SIZE / 512))
((SECTOR_COUNT++))

nasm -DSECTOR_COUNT=${SECTOR_COUNT} -f bin -o ${2} ${1}

exit 0
