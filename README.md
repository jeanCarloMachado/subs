## subs

This project aims to reduce the amount of things we type.


## Examples:

The given code:

```
cl dog
    co color
    pf bite $person
        $person->injuried = true;
    pf bark $people
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
    public function bite ($person)
    {
          $person->injuried = true;
    }
    public function bark ($people)
    {
          foreach ($people as $person) {
                if(person->isbad()) {
                      $this->bark()
            }
        }
    }
}

```


Given this configuration on the file ~/subsconfig.ini:

```ini
[global]
pf=public function %s (%s)\n{\n%c\n}
co=public function __constructor($%s)\n{\n%c\n}
cl=class %s\n{\n%c\n}
fe=foreach ($%s as $%s) {\n%c\n}
if=if(%s) {\n%c\n}
```

## Installation:

```sh
sudo wget -O /usr/local/bin/subs "https://www.dropbox.com/s/l3i53osfrrs5mhr/subs" && chmod +x /usr/local/bin/subs
```


## Usage

After having your config just pipe the text to stdin.

```sh
subs <<< 'cl Gandalf'
class Gandalf\n{\n\n}
```


## Integration

The true power of subs comes when you integrate it on your text editor.

For vim, using Tim Pope's textobject integration is simply a matter of:

```vimscript
fun! s:Subs(str)
  let my_filetype = &filetype
  let out = ChompedSystemCall('runFunction subsEval', a:str."\n")
  return out
endfunc
call MapAction('Subs', '<leader>y')
```

