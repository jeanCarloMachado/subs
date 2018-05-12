# Documentation


## Replaceable keys

 - %s - a string argument, consumed in order
 - %1,%2,%n - a positional argument, meaning that all occurrences of %1 will be replaced by the first argument
 - %i - a indentation unit, uses the same as the top-level indentation
 - %c - expression content


## Program options

-h      to get the program help
-p      to set the preferred section, useful to get only say, python snippets instead of php




## Examples:
### The given code:

```
cl Dog
    co color
        as bark false
    pf bark people
        fe people person
            if person->isbad()
                as bark true
    //no snippet match so keep literal
    pf bite person
        ao person injuried true
    set color
    get color
```

### Produces:


```javascript
class Dog
{
    constructor(color) {
        this.color = color
        this.bark = false
    }
    bark(people) {
        people.forEach((person) => {
            if (person.isbad()) {
                this.bark = true
            }
        })
    }
    //no snippet match so keep literal
    bite(person) {
        person.injuried = true
    }
    set color(color) {
        this.color = color
    }
    get color() {
        this.color
    }
}
```

### Or

```php
class Dog
{
    public function __construct($color)
    {
        $this->color = $color;
        $this->bark = false;
    }
    public function bark ($people)
    {
        foreach ($people as $person) {
            if($person->isbad()) {
                $this->bark = true;
            }
        }
    }
    //no snippet match so keep literal
    public function bite ($person)
    {
        $person->injuried = true;
    }
    public function setcolor($color)
    {
        $this->color = $color;
    }
    public function getcolor()
    {
        return$this->color;
    }
}
```

### Or


```python
class Dog:
    def __init__(self, color):
        self.color = color
    def bark (people):
        for people in person:
            if person.isbad():
                self.bark = true
    //no snippet match so keep literal
    def bite (person):
        person.injuried = true
    def setcolor(self,color):
        self.color = color
    def getcolor(self):
        return self.color
```


Given the proper filetype.
