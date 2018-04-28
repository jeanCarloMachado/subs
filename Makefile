.PHONY: test

all: compile install

install:
	stack install
	cp -rf /home/jean/.local/bin/subs /home/jean/Dropbox

compile:
	stack build

test: compile
	./run_tests.sh


deploy:
	upload_and_share /home/jean/Dropbox/subs

