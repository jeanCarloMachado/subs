## subs

This project aims to reduce the amount of things we type.


## Examples:


Given this rules on the config file default.ini:

```ini
pf=public function %s (%s)\n{\n%c\n}
co=public function __constructor($%s)\n{\n%c\n}
cl=class %s\n{\n%c\n}
fe=foreach ($%s as $%s) {\n%c\n}
if=if(%s) {\n%c\n}
```

The given code:

```
<?php


cl dog
  co color
  pf bite person
  pf bark people
    fe people person
      if person->isbad()
        $this->bark()

```

Produces:


```php
class dog
{
  public function __constructor($color)
  {
  
  }
  public function bite (person)
  {
  
  }
  public function bark (people)
  {
      foreach ($people as $person) {
            if(person->isbad()) {
                            $this->bark()
            }
      }
  }
}

```

