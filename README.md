## subs


This project aims to reduce the amount of things we type.

We are used to messy languages and they could be improved.

## Examples:


```
<?php


cl person
  pf isBad name
  pf isBad name


cl dog
  co color
  pf bite person
  pf bark people
    fe people person
      if person->isbad()
        $this->bark()

```

