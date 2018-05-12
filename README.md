## subs

This project aims to reduce the amount of things we type. Specially
useful for syntax heavy languages.

![build status](https://travis-ci.org/jeanCarloMachado/subs.svg?branch=master)
[![Join the chat at https://gitter.im/subs-subs/Lobby](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/subs-subs/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)


![example](https://i.imgur.com/HDwTl6V.gif)


## Installation:

### Linux
```sh
sudo wget -O /usr/local/bin/subs "http://jeancarlomachado.net/subs" \
&& sudo chmod +x /usr/local/bin/subs
```


### Other

You have to compile for your platform, the only requirement is Haskell stack.

```sh
git clone git@github.com:jeanCarloMachado/subs.git
cd subs
make
make install
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
  let out = system('subs -p '.my_filetype, a:str."\n")
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
