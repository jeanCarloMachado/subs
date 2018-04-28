#!/bin/sh

test_expect_same() {
    test "$2" = "$3"
    isEqual=$?
    test_expect_success "$1" "
        test $isEqual = 0
    "
}

test_description="Basic features"

. sharness.sh

result=$(echo 'myMail' | subs)

myMail
test_expect_success "email" "
    test $result = contato@jeancarlomachado.com.br
"

test_expect_same "replaces complex line" "$(echo 'af a' | subs)" 'function ($a)\n{\n\n}'

test_expect_same "replaces partial arguments keep rest empty" "$(echo 'pf a' | subs)" 'public function a ()\n{\n\n}'

test_expect_same "substitute variable" "$(echo 'cl Car' | subs)" 'class Car\n{\n\n}'

result=$(subs <<< 'cl Car
 pf turnOn')

test_expect_same "nested" "$result" 'class Car\n{\n public function turnOn ()\n {\n \n }\n}' ;


result=$(subs <<< 'cl dog
  co color
  pf bite person
    $person->isHurt();
  pf bark people
    fe people person
      if person->isbad()
        $this->bark()')

test_expect_same "complex nested" "$result" 'class dog\n{\n  public function __constructor($color)\n  {\n  \n  }
  public function bite (person)\n  {\n          $person->isHurt();\n  }
  public function bark (people)\n  {\n      foreach ($people as $person) {\n            if(person->isbad()) {\n                            $this->bark()\n            }\n      }\n  }\n}' ;


test_done


myMail
