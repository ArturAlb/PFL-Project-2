# Project 2
>       Group: T11_G07

>   (50%) up202006479 - Ruben Silverio Fernandes Esteves

>   (50%) up202108663 - Artur Jose Albuquerque Oliveira

## Execution

The code can be executed ran under GHCi version 9.0
or later and by using the following commands:

```bash
#compile
ghc --make main.hs

#run
./main
```

## Project summary

This project is able to take a program in the form of a string, for example:
```
"i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);"
```
And run it, returning the result:

```
("","fact=3628800,i=1")
```

## Project description