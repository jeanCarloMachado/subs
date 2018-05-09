## subs

This project aims to reduce the amount of things we type.


## Examples:
The given code:

```
cl dog
    pi bark
    pi color
    co color
      si bark false
    pf bite person
        $person->injuried = true;
    pf bark people
        fe people person
            if person->isbad()
                si bark true
    set color
```

Produces:

```php
class dog
{
    private $bark;
    private $color;
    public function __constructor($color)
    {
        $this->color = $color;
        $this->bark = false;
    }
    public function bite($person)
    {
        $person->injuried = true;
    }
    public function bark($people)
    {
        foreach ($people as $person) {
            if ($person->isbad()) {
                $this->bark = true;
            }
        }
    }
    public function setcolor($color)
    {
        $this->color = $color;
    }
}
```

Given the default config. file.


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
