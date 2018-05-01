#!/bin/sh


test_description="Basic features"

. sharness.sh

test_expect_same "same email" "$(echo 'myMail' | subs)" "myMail@gmail.com"

test_expect_same "replaces complex line" "$(echo 'af a' | subs)" 'function ($a)\n{\n\n}'

test_expect_same "replaces partial arguments keep rest empty" "$(echo 'pf a' | subs)" 'public function a ()\n{\n\n}'

test_expect_same "substitute variable" "$(echo 'cl Car' | subs)" 'class Car\n{\n\n}'


test_done


