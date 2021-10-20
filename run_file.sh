#!/bin/bash

file=$1

[ ! -e "$file" ] && echo "$file does not exist." && exit 1

filename=$(basename "$file" .wacc)
arm-linux-gnueabi-gcc -o "${filename}" -mcpu=arm1176jzf-s -mtune=arm1176jzf-s "${filename}.s"
qemu-arm -L /usr/arm-linux-gnueabi/ "${filename}"
