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

### Example session
```
>> let D = (\x. x x) in let F = (\f. f (f y)) in D (F ())
y y
>> let T = (\f x. f (f x)) in (\f x. T (T (T (T T))) f x) () y
y
>> \x. \y. y x
\x. \y. y:0 x:1
>> ^C
```

## Notation
[Conventional Lambda Calculus notation applies](https://en.wikipedia.org/wiki/Lambda_calculus_definition#Notation),
with the exception that variable names are mmultiple characters long,
and `\` is used in lieu of `λ` for convenience.

* Variable names are alphanumeric, beginning with a letter.
* Outermost parentheses may be dropped: `M N` is equivalent to `(M N)`.
* Applications are left-associative: `M N P` may be written instead of `((M N) P)`.
* The body of an abstraction extends as far right as possible: `\x. M N` means `\x.(M N)` and not ``(\x. M) N`.
* A sequence of abstractions may be contracted: `\foo. \bar. \baz. N` may be abbreviated as `\foo bar baz. N`.
* Variables may be bound using let expressions: `let x = N in M` abbreviates `(\x. N) M`.

### Violations of convention
* I use spaces to separate variables in abstractions instead of commas because I think it looks better.

### Additional extensions to notation
Since `\x. x` is the left identity of applications and application syntax is left-associative,
I (syntactically) permit unary and nullary applications so that `()` is `\x. x`, and `(x)` is `x`.

On the same principle, the syntax of a lambda of no variables `\. e` is `e`.
