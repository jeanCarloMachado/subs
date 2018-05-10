#!/bin/bash


test_description="Nesting"

. sharness.sh


export SUBS_CONFIG=/tmp/test-nesting.ini
cat > $SUBS_CONFIG <<EOF
[global]
pf=public function %s (%s)\n{\n\n}
cl=class %s\n{\n%c\n}
co=public function __construct($%1)\n{\n%i\$this->%1 = $%1;\n%c\n}
EOF



subsCompare "simple nesting" "cl Car
 pf turnOn" \
'class Car
{
 public function turnOn ()
 {
 
 }
}'

subsCompare "keep indention 4 spaces" "cl Car
    pf turnOn" \
'class Car
{
    public function turnOn ()
    {
    
    }
}'

subsCompare "same identation as sibling" "cl Car
pf turnOn" \
'class Car
{

}
public function turnOn ()
{

}'

subsCompare "set indentation for multi line nested snippets" \
'cl Car
    co foo' \
'class Car
{
    public function __construct($foo)
    {
        $this->foo = $foo;
    
    }
}'

test_done
