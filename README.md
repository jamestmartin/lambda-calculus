# untyped-lambda-calculus
A simple implementation of the untyped lambda calculus as an exercise.

This implementation lacks many features necessary to be useful,
for example `let` bindings, built-in functions, binding free variables, or a good REPL.
This project purely exists as an exercise and is not intended for general use.
I will make useful programming languages in the future, but this is not one of them.

## Usage
Type in your expression at the prompt: `>> `.
The result of the evaluation of that expression will then be printed out.
Exit the prompt with `Ctrl-C` (or however else you kill a program in your terminal).

Bound variables will be printed followed by a number representing the number of binders
between it and its definition for disambiguation.
This is not guaranteed not to capture free variables.

### Examples
```
>> (\D F I. D (F I)) (\x. x x) (\f. f (f y)) (\x. x)
y y
>> (\T f x. T (T (T (T T))) f x) (\f x. f (f x)) (\x. x) y
y
>> \x. \y. y x
\x. \y. y:0 x:1
```

## Syntax
* Variables are alphanumeric, beginning with a letter.
* Applications are left-associative, and parentheses are not necessary.
* Lambdas are represented by `\\`, bind as far right as possible, and parentheses are not necessary.

### Examples
* Variables: `x`, `abc`, `D`, `fooBar`, `f10`
* Applications: `(\x. x x) y`, `a b`, `((g f) y)`
* Lambdas: `\x. N`, `\x y. y`, `(\x. f x)`