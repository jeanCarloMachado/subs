#!/bin/bash



test_description="Basic features"

. sharness.sh


export SUBS_CONFIG=/tmp/test-basic.ini
cat > $SUBS_CONFIG <<EOF
[global]
myMail=myMail@gmail.com
fn=function ($%s){\n\n}
pf=public function %s (%s)\n{\n\n}
cl=class %s\n{\n\n}
EOF


test_expect_same "same email" "$(echo 'myMail' | subs)" "myMail@gmail.com"

test_expect_same "replaces multiline line" "$(echo 'fn a' | subs)" \
'function ($a){

}'

test_expect_same "substitute variable" "$(echo 'cl Car' | subs)" \
'class Car
{

}'

test_expect_same "replaces partial arguments keep rest empty" "$(echo 'pf a' | subs)" \
'public function a ()
{

}'

test_expect_same "remove previous empty spaces" "$(printf "\n\npf a" | subs)" \
'public function a ()
{

}'




test_done


