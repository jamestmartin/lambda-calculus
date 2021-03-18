# Lambda Calculus
This is a simple programming language derived from lambda calculus,
using the Hindley-Milner type system, plus some builtin types, `fix`, and `callcc`

## Usage
Run the program using `stack run` (or run the tests with `stack test`).

Type in your expression at the prompt: `>> `. This will happen:
* the type for the expression will be inferred and then printed,
* then, regardless of whether typechecking succeeded, expression will be evaluated to normal form using the call-by-value evaluation strategy and then printed.

Exit the prompt with `Ctrl-c` (or equivalent).

## Syntax
The parser's error messages currently are virtually useless, so be very careful with your syntax.

* Variable names: any sequence of letters.
* Function application: `f x y`
* Lambda abstraction: `\x y z. E` or `λx y z. E`
* Let expressions: `let x = E; y = F in G`
* Parenthetical expressions: `(E)`
* Constructors: `()`, `(x, y)` (or `(,) x y`), `Left x`, `Right y`, `Z`, `S`, `[]`, `(x :: xs)` (or `(:) x xs`), `Char n`.
  * The parentheses around the cons constructor are not optional.
  * `Char` takes a natural number and turns it into a character.
* Pattern matchers: `case { Left a -> e ; Right y -> f }`
  * Pattern matchers can be applied like functions, e.g. `{ Z -> x, S -> y } 10` reduces to `y`.
  * Patterns must use the regular form of the constructor, e.g. `(x :: xs)` and not `((::) x xs)`.
  * There are no nested patterns or default patterns.
  * Incomplete pattern matches will crash the interpreter.
* Literals: `1234`, `[e, f, g, h]`, `'a`, `"abc"`
  * Strings are represented as lists of characters.
* Type annotations: there are no type annotations; types are inferred only.

## Types
Types are checked/inferred using the Hindley-Milner type inference algorithm.

* Functions: `a -> b` (constructed by `\x. e`)
* Products: `a * b` (constructed by `(x, y)`)
* Unit: `★` (constructed by `()`)
* Sums: `a + b` (constructed by `Left x` or `Right y`)
* Bottom: `⊥` (currently useless because incomplete patterns are allowed)
* The natural numbers: `Nat` (constructed by `Z` and `S`)
* Lists: `List a` (constructed by `[]` and `(x :: xs)`)
* Characters: `Char` (constructed by `Char`, which takes a `Nat`)
* Universal quantification (forall): `∀a b. t`

## Builtins
Builtins are variables that correspond with a built-in language feature
that cannot be replicated by user-written code.
They still are just variables though; they do not receive special syntactic treatment.

* `fix : ∀a. ((a -> a) -> a)`: an alias for the strict fixpoint combinator that the typechecker can typecheck.
* `callcc : ∀a b. (((a -> b) -> a) -> a)`: [the call-with-current-continuation control flow operator](https://en.wikipedia.org/wiki/Call-with-current-continuation).

Continuations are printed as `λ!. ... ! ...`, like a lambda abstraction
with an argument named `!` which is used exactly once;
however, continuations are *not* the same as lambda abstractions
because they perform the side effect of modifying the current continuation,
and this is *not* valid syntax you can input into the REPL.

## Example code
Create a list by iterating `f` `n` times:
```
fix \iterate f x. { Z -> [] ; S n -> (x :: iterate f (f x) n) }
: ∀e. ((e -> e) -> (e -> (Nat -> [e])))
```

Use the iterate function to count to 10:
```
>> let iterate = fix \iterate f x. { Z -> [] ; S n -> (x :: iterate f (f x) n) }; countTo = iterate S 1 in countTo 10
: [Nat]
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
```

Append two lists together:
```
fix \append xs ys. { [] -> ys ; (x :: xs) -> (x :: append xs ys) } xs
: ∀j. ([j] -> ([j] -> [j]))
```

Reverse a list:
```
fix \reverse. { [] -> [] ; (x :: xs) -> append (reverse xs) [x] }
: ∀c1. ([c1] -> [c1])
```

Putting them together so we can reverse `"reverse"`:
```
>> let append = fix \append xs ys. { [] -> ys ; (x :: xs) -> (x :: append xs ys) } xs; reverse = fix \reverse. { [] -> [] ; (x :: xs) -> append (reverse xs) [x] } in reverse "reverse"
: [Char]
"esrever"
```

Calculating `3 + 2` with the help of Church-encoded numerals:
```
>> let Sf = \n f x. f (n f x); plus = \x. x Sf in plus (\f x. f (f (f x))) (\f x. f (f x)) S Z
: Nat
5
```

This expression would loop forever, but `callcc` saves the day!
```
>> S (callcc \k. (fix \x. x) (k Z))
: Nat
1
```

And if it wasn't clear, this is what the `Char` constructor does:

```
>> { Char c -> Char (S c) } 'a
: Char
'b
```

Here are a few expressions which don't typecheck but are handy for debugging the evaluator:
```
>> let D = \x. x x; F = \f. f (f y) in D (F \x. x)
y y
>> let T = \f x. f (f x) in (\f x. T (T (T (T T))) f x) (\x. x) y
y
>> (\x y z. x y) y
λy' z. y y'
```
