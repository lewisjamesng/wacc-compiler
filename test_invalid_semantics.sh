#!/bin/bash
YELLOW='\033[1;33m'
expected='exit:200'
unexpected1='exit:0'
unexpected2='exit:100'
errors=0

echo -e "${YELLOW}Invalid Semantic Tests"

for file in ./src/test/invalid/semanticErr/**/*.wacc
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

