.PHONY: test

compile:
	ghc --make subs.hs -dynamic

test: compile
	./run_tests.sh

run: compile
	./subs
