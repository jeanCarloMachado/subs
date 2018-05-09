
test_description="numeric positioning"

. sharness.sh


export SUBS_CONFIG=/tmp/test-positioning.ini
cat > $SUBS_CONFIG <<EOF
[global]
set=public function set%1($%1)\n{\n\$this->%1 = \$%1;\n}
EOF


test_expect_same "numeric position" "$(printf "set gandalf" | subs)" \
'public function setgandalf($gandalf)
{
$this->gandalf = $gandalf;
}'


test_done
