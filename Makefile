.PHONY: test

compile:
	ghc --make subs.hs -dynamic

test: compile
	./run_tests.sh

run: compile
	echo 'fn'  | ./subs

compileHard:
	ghc --make subs.hs
