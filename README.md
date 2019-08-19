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

## Differences from the traditional lambda calculus
This version of the lambda calculus is completely compatible
with the traditional lambda calculus, but features a few extensions.

### Nullary applications
In any binary application `(f x)`, the `f` is a function and the `x` is a variable.
Applications are left-associative, meaning `(f x y)` equals `((f x) y)`.
Any term `x` may be expanded to an application `(id x)`.
Working backwards, we have `(f x y)` equals `(((id f) x) y)`.
Thus we may reasonably say that `() = id`
and thus that `(f x y)` equals `(((() f) x) y)`,
and that `(x)` equals `(() x)`, which reduces to just `x`.

### Nullary functions and generalized eta reductions
We can apply a similar argument to the function syntax.
`(\x y z. E)` is the same as `(\x.(\y.(\z.E)))` because lambda is right-associative.
Any term `x` may be eta-expanded into a lambda `(\y. ((() x) y))`.
Working backwards, we have `(\x. (() x))` eta-reducing to `()`.
Therefore, the identity function eta-reduces to just `()`.

Again similarly we have the nulladic lambda syntax `(\.E)`
which trivially beta-reduces to `E`.

I also take any series of ordered applications
`(\v v1 v2 ... vn. v:n v2:(n-1) ... v(n-1):1 vn:0)`
to eta-reduce to `()` (including `()` itself and, trivially, `(\x. x)`).

### Nullary functions

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
* Lambdas are represented by `\`, bind as far right as possible, and parentheses are not necessary.

### Examples
* Variables: `x`, `abc`, `D`, `fooBar`, `f10`
* Applications: `(\x. x x) y`, `a b`, `((g f) y)`
* Lambdas: `\x. N`, `\x y. y`, `(\x. f x)`