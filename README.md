# Latte compiler

It depends on two monad types: reader and exept so far.

## Frontend

In type checker i use both monads, i store exceptions on exept monad and map from identifier into type in reader one.

<!-- ## Dzuga Interpreter

In interpreter i also use both monads, i use the same way of storing exceptions and i store map from identifier into specific value like bool, int, string, function and array. -->

# Running

You need to have: BNFC, ghci and make installed on your computer to run it.

### Compile program with

```sh
make
```

### Run

```sh
compiler.exe <path_to_latte_program>
```
