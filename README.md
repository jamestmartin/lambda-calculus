# Lambda Calculus
This is a simple implementation of the untyped lambda calculus
with an emphasis on clear, readable Haskell code.

## Usage
Run the program using `stack run` (or run the tests with `stack test`).

Type in your expression at the prompt: `>> `.
The expression will be evaluated to normal form using the call-by-value evaluation strategy and then printed.
Exit the prompt with `Ctrl-c` (or equivalent).

### Example session
```
>> let D = \x. x x; F = \f. f (f y) in D (F \x. x)
y y
>> let T = \f x. f (f x) in (\f x. T (T (T (T T))) f x) (\x. x) y
y
>> (\x y z. x y) y
λy' z. y y'
>> let fix = (\x. x x) \fix f x. f (fix fix f) x; S = \n f x. f (n f x); plus = fix \plus x. x S in plus (\f x. f (f (f x))) (\f x. f (f x)) f x
f (f (f (f (f x))))
>> y (callcc \k. (\x. (\x. x x) (\x. x x)) (k z))
y z
>> ^C
```

## Notation
[Conventional Lambda Calculus notation applies](https://en.wikipedia.org/wiki/Lambda_calculus_definition#Notation),
with the exception that variable names are multiple characters long,
`\` is permitted in lieu of `λ` to make it easier to type,
and spaces are used to separate variables rather than commas.

* Variable names are alphanumeric, beginning with a letter.
* Outermost parentheses may be dropped: `M N` is equivalent to `(M N)`.
* Applications are left-associative: `M N P` may be written instead of `((M N) P)`.
* The body of an abstraction or let expression extends as far right as possible: `\x. M N` means `\x.(M N)` and not `(\x. M) N`.
* A sequence of abstractions may be contracted: `\foo. \bar. \baz. N` may be abbreviated as `\foo bar baz. N`.
* Variables may be bound using let expressions: `let x = N in M` is syntactic sugar for `(\x. N) M`.
* Multiple variables may be defined in one let expression: `let x = N; y = O in M`

## Call/CC
This interpreter has preliminary support for
[the call-with-current-continuation control flow operator](https://en.wikipedia.org/wiki/Call-with-current-continuation).
However, it has not been thoroughly tested.

To use it, simply apply the variable `callcc` like you would a function, e.g. `(callcc (\k. ...))`.
`callcc` is not a normal variable and cannot be shadowed;
`\callcc. callcc` is *not* the identity function, it *ignores* its argument and then returns the *operator* `callcc`.

Continuations are printed as `λ!. ... ! ...`, like a lambda abstraction
with an argument named `!` which is used exactly once;
however, continuations are *not* the same as lambda abstractions
because they perform the side effect of modifying the current continuation,
and this is *not* valid syntax you can input into the REPL.
