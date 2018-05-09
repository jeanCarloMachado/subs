#!/bin/bash

test_description="numeric positioning"

. sharness.sh


export SUBS_CONFIG=/tmp/test-positioning.ini
cat > $SUBS_CONFIG <<EOF
[global]
set=public function set%1($%1)\n{\n\$this->%1 = \$%1;\n}
EOF


subsCompare "numeric position" "set gandalf" \
'public function setgandalf($gandalf)
{
$this->gandalf = $gandalf;
}'


test_done
