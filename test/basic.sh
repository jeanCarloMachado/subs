#!/bin/sh

test_description="Basic features"

. sharness.sh

result=$(echo 'myMail' | subs)

test_expect_success "email" "
    test $result = contato@jeancarlomachado.com.br
"

result=$(echo 'fn a b c' | subs)

test_expect_success "func" "
    test $result = 'function a (\$b) : c'"

test_done
