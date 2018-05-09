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
	aws s3 cp /home/jean/Dropbox/subs s3://jeancarlomachado.site/subs

