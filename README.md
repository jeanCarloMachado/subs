## subs

This project aims to reduce the amount of things we type.

![build status](https://travis-ci.org/jeanCarloMachado/subs.svg?branch=master)


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
        self.bark = bark
    }
    bark(people) {
        for people in person:
            if (person->isbad()) {
                self.bark = bark
            }
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
        $this->bark = bark;
    }
    public function bark ($people)
    {
        foreach ($people as $person) {
            if(person->isbad()) {
                $this->bark = bark;
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
                self.bark = bark
    //no snippet match so keep literal
    def bite (person):
        person.injuried = true
    def setcolor(self,color):
        self.color = color
    def getcolor(self):
        return self.color
```


Given the proper filetype.


## Installation (Linux only):

```sh
sudo wget -O /usr/local/bin/subs "http://jeancarlomachado.net/subs" \
&& sudo chmod +x /usr/local/bin/subs

```

Create/copy the config file  into `~/.subsconfig.ini`. [This file](https://raw.githubusercontent.com/jeanCarloMachado/subs/master/EXAMPLE.subsconfig.ini) is the default
one.



## Usage

After having your config just pipe the text to stdin.

```sh
subs <<< 'cl Gandalf'
class Gandalf
{

}
```


## Integrations

The true power of subs comes when you integrate it with your tools.


### Vim

For vim, using Tim Pope's textobject integration is simply a matter of:

```vimscript
fun! ChompedSystemCall( ... )
  return substitute(call('system', a:000), '\n\+$', '', '')
endfun


fun! s:Subs(str)
  let my_filetype = &filetype
  let out = ChompedSystemCall('subs -p '.my_filetype, a:str."\n")
  return out
endfunc
call MapAction('Subs', '<leader>y')
```

### Zsh


```sh
# in your .zshrc
evaluate-snippets-selection () {
    if [[ $CURSOR -gt $MARK ]]; then
        start=$MARK
        end=$(( CURSOR + 1 ))
    else
        start=$(( CURSOR + 1 ))
        end=$MARK
    fi
    BUFFER="$BUFFER[0,start]$(subs <<< $BUFFER[start+1,end])$BUFFER[end+1,-1]"
}
zle -N evaluate-snippets-selection
bindkey '^O' evaluate-snippets-selection
```


### Dmenu

```sh
#!/bin/bash
# file dmenuSnippets.sh
key=$( subs -k | rofi -levenshtein-sort -dmenu -p "snippet: ")
echo $key
value=$( echo "$key" | subs )

echo "$value"
mycopy "$value"
notify-send "Copied"
```
