#!/bin/bash


test_description="Nesting"

. sharness.sh


export SUBS_CONFIG=/tmp/test-nesting.ini
cat > $SUBS_CONFIG <<EOF
[global]
pf=public function %s (%s)\n{\n\n}
cl=class %s\n{\n%c\n}
EOF



subsCompare "simple nesting" "cl Car
 pf turnOn" \
'class Car
{
 public function turnOn ()
 {
 
 }
}'


test_done
