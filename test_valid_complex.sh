#!/bin/bash
PURPLE='\033[0;35m'
expected='exit:0'
unexpected1='exit:100'
unexpected2='exit:200'
errors=0

echo -e "${PURPLE}Valid Tests"

for file in ./src/test/valid/complex/**/*.wacc
do
  result=$(./compile "$file" "ARM")
  if echo "$result" | grep -q $expected; then
    echo "----Passed. $file"
  elif echo "$result" | grep -q $unexpected1; then
    echo "Test failed. Expected: $expected. Found: $unexpected1. $file"
    errors=$((errors+1))
  else
    echo "Test failed. Expected: $expected. Found: $unexpected2. $file"
    errors=$((errors+1))
  fi
done
echo "Test finished. Summary:"
if [ "$errors" -gt 0 ]; then
  echo "There were $errors errors found."
  exit 1
else
  echo "Passed all test cases."
  exit 0
fi

