.PHONY: test

all: compile install

install:
	stack install

compile:
	stack build

test: compile
	./run_tests.sh

