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


subsCompare "same email" "myMail" "myMail@gmail.com"

subsCompare "replaces multiline line" "fn a" \
'function ($a){

}'

subsCompare "substitute variable" "cl Car" \
'class Car
{

}'

subsCompare "replaces partial arguments keep rest empty" "pf a" \
'public function a ()
{

}'

subsCompare "remove previous empty spaces" \
'

pf a' \
'public function a ()
{

}'



test_done


