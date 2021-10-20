#!/bin/bash
CYAN='\033[0;36m'
expected='exit:100'
unexpected1='exit:200'
unexpected2='exit:0'
errors=0

echo -e "${CYAN}Invalid Syntax Tests"

for file in ./src/test/invalid/syntaxErr/**/*.wacc
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

