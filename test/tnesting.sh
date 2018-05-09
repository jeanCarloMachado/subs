#!/bin/sh


test_description="Nesting"

. sharness.sh


# test_expect_same "nested" "$(printf "cl Car\n pf turnOn" | subs)" 'class Car\n{\n public function turnOn ()\n {\n \n }\n}' ;


# result=$(echo 'cl dog
#   co color
#   pf bite person
#     $person->isHurt();
#   pf bark people
#     fe people person
#       if person->isbad()
#         $this->bark()' | subs)

# test_expect_same "complex nested" "$result" 'class dog\n{\n  public function __constructor($color)\n  {\n  $this->color = $color;\n  \n  }
#   public function bite (person)\n  {\n          $person->isHurt();\n  }
#   public function bark (people)\n  {\n      foreach ($people as $person) {\n          if(person->isbad()) {\n                      $this->bark()\n      }\n    }\n  }\n}';


test_done
