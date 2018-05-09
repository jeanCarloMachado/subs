#!/bin/bash


test_description="Sections"

. sharness.sh


export SUBS_CONFIG=/tmp/test-sections.ini
cat > $SUBS_CONFIG <<EOF
[random]
myMail=myMail@gmail.com
[javascript]
cl=var %s = class {}
[php]
cl=class %s {}
EOF


subsCompare "random section name" "myMail" "myMail@gmail.com"

subsCompare "selects php when type is passed" "cl foo" "class foo {}" -p 'php'

subsCompare "selects javascript when type is passed " "cl foo" "var foo = class {}" -p 'javascript'

subsCompare "not in section but in file" "myMail" "myMail@gmail.com" -p 'javascript'

test_done


