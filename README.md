# James Martin's Lambda Calculus
This is a simple implementation of the untyped lambda calculus
with an emphasis on clear, readable Haskell code.

## Usage
Type in your expression at the prompt: `>> `.
The expression will be evaluated to normal form and then printed.
Exit the prompt with `Ctrl-c` (or equivalent).

### Example session
```
>> let D = \x. x x; F = \f. f (f y) in D (F \x. x)
y y
>> let T = \f x. f (f x) in (\f x. T (T (T (T T))) f x) (\x. x) y
y
>> (\x y z. x y) y
λy' z. y y'
>> ^C
```

## Notation
[Conventional Lambda Calculus notation applies](https://en.wikipedia.org/wiki/Lambda_calculus_definition#Notation),
with the exception that variable names are multiple characters long,
`\` is used in lieu of `λ` to make it easier to type,
and spaces are used to separate variables rather than commas.

* Variable names are alphanumeric, beginning with a letter.
* Outermost parentheses may be dropped: `M N` is equivalent to `(M N)`.
* Applications are left-associative: `M N P` may be written instead of `((M N) P)`.
* The body of an abstraction or let expression extends as far right as possible: `\x. M N` means `\x.(M N)` and not `(\x. M) N`.
* A sequence of abstractions may be contracted: `\foo. \bar. \baz. N` may be abbreviated as `\foo bar baz. N`.
* Variables may be bound using let expressions: `let x = N in M` is syntactic sugar for `(\x. N) M`.
* Multiple variables may be defined in one let expression: `let x = N; y = O in M`
