#!/bin/sh



test_description="Basic features"

. sharness.sh

echo '[global]
myMail=myMail@gmail.com
fn=function ($%s){\n\n}
' > /tmp/config.ini

export SUBS_CONFIG=/tmp/config.ini


test_expect_same "same email" "$(echo 'myMail' | subs)" "myMail@gmail.com"

test_expect_same "replaces multiline line" "$(echo 'fn a' | subs)" \
'function ($a){

}'

# test_expect_same "replaces partial arguments keep rest empty" "$(echo 'pf a' | subs)" 'public function a ()\n{\n\n}'

# test_expect_same "substitute variable" "$(echo 'cl Car' | subs)" 'class Car\n{\n\n}'


test_done


