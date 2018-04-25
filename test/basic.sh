#!/bin/sh

test_expect_same() {
    test "$2" =  "$3"
    isEqual=$?
    test_expect_success "$1" "
        test $isEqual = 0
    "
}

test_description="Basic features"

. sharness.sh

result=$(echo 'myMail' | subs)

test_expect_success "email" "
    test $result = contato@jeancarlomachado.com.br
"


result=$(echo 'fn' | subs)
test_expect_same "replaces complex line" "$result" 'public function %s ()\n{\n\n}'


result=$(echo 'cl Car' | subs)
test_expect_same "substitute variable" "$result" 'class Car\n{\n\n}'


test_done


myMail
