#!/bin/sh

test_description="Basic features"

. sharness.sh

result=$(echo 'myMail' | subs  --config 'myMail=contato@jeancarlomachado.com.br')
echo $result

test_expect_success "email" "
    test $result = contato@jeancarlomachado.com.br
"

test_done
