.PHONY: test

compile:
	stack build

test: compile
	./run_tests.sh

run: compile
	echo 'fn'  | subs
