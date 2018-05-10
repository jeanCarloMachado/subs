#!/bin/bash

test_description="numeric positioning"

. sharness.sh


export SUBS_CONFIG=/tmp/test-positioning.ini
cat > $SUBS_CONFIG <<EOF
[global]
set=public function set%1($%1)\n{\n\$this->%1 = \$%1;\n}
for=for (let %2 = 0; %2 < %1.length; %2++) {\n%c\n}
EOF


subsCompare "numeric position" "set gandalf" \
'public function setgandalf($gandalf)
{
$this->gandalf = $gandalf;
}'

subsCompare "multiple arguments" "for foo bar" \
'for (let bar = 0; bar < foo.length; bar++) {

}'


test_done
