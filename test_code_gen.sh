#!/bin/bash

expected="expected.txt"
actual="actual.txt"
error=false
input="input.txt"

make

for f in ./src/test/valid/simple/*/*.wacc
do
  filename=$(basename "$f" .wacc)
  echo "-------------------------"

  cmd="curl -s -F \"options[]=-x\" -F \"run=\" -F \"testfile=@$f\" \"https://teaching.doc.ic.ac.uk/wacc_compiler/run.cgi\""

  response=$(eval "$cmd")

  (echo "$response" | jq -r '.compiler_out') > $expected
  sed -i -n '/===========================================================/,/===========================================================/{/===========================================================/!{/===========================================================/!p}}' $expected

  ./compile "$f ARM unoptimised"

  if [[ -e "${filename}.s" ]]; then
    arm-linux-gnueabi-gcc -o "${filename}" -mcpu=arm1176jzf-s -mtune=arm1176jzf-s "${filename}.s"
    (qemu-arm -L /usr/arm-linux-gnueabi/ "${filename}" < $input) > $actual

    if cmp -s "$actual" "$expected"; then
      printf 'Expected output generated for %s\n' "$f"
    else
      printf 'Unexpected output generated for %s\n' "$f"
      error=true
    fi
  else
    echo "no assembly file was generated ${filename}.s\n"
  fi
done

make clean
rm $actual $expected

$error && exit 1
exit 0