#!/bin/bash


test_description="Sections"

. sharness.sh


export SUBS_CONFIG=/tmp/test-sections.ini
cat > $SUBS_CONFIG <<EOF
[random]
myMail=myMail@gmail.com
EOF


subsCompare "random section name" "myMail" "myMail@gmail.com"


test_done


