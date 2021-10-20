all: rules

rules:
	sbt assembly
test: rules test-valid-simple test-valid-complex test-syntax test-semantics test-back-end-output test-valid-simple-x86 test-valid-complex-x86
test-valid-simple:
	bash test_valid_simple.sh
test-valid-complex:
	bash test_valid_complex.sh
test-syntax:
	bash test_invalid_syntax.sh
test-semantics:
	bash test_invalid_semantics.sh
test-back-end-output:
	bash test_code_gen.sh
test-valid-simple-x86:
	bash test_valid_simple_x86.sh
test-valid-complex-x86:
	bash test_valid_complex_x86.sh
clean:
	sbt clean
	find . -name "*.s" -type f -delete
	find . -type f -executable -exec sh -c "file -i '{}' | grep -q 'x-executable; charset=binary'" \; -print | xargs rm -f

.PHONY: all rules test-valid test-syntax test-semantics test-back-end-output clean
