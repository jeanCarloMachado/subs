## sub 


This project aims to reduce the amount of things we type.

We are used to messy languages and they could be improved.

## Examples:





```
--eval
fn a b

--config

public function %1 ($%2) :%3
{

}

--result

public function a ($b) :c
{

}
```

And

```
--eval
cl Door

--config
class %1
{
    %nested
}

--result
class Door {}
```

## Nesting things




```
--eval
cl Door
    fn open doorId void

--config
no extra config

--result

class Door
{
    public function open($doorId) : void
    {
    }
}
```


Could be used as well to more mundane things, like simple substitutions


```
--eval
myMail

--config
contato@jeancarlomachado.com.br

--result

contato@jeancarlomachado.com.br
```


