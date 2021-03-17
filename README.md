# Lambda Calculus
This is a simple programming language derived from lambda calculus.

## Usage
Run the program using `stack run` (or run the tests with `stack test`).

Type in your expression at the prompt: `>> `.
The expression will be evaluated to normal form using the call-by-value evaluation strategy and then printed.
Exit the prompt with `Ctrl-c` (or equivalent).

## Syntax
The parser's error messages currently are virtually useless, so be very careful with your syntax.

* Variable names: any sequence of letters.
* Function application: `f x y`
* Lambda abstraction: `\x y z. E` or `λx y z. E`
* Let expressions: `let x = E; y = F in G`
* Parenthetical expressions: `(E)`
* Constructors: `()`, `(x, y)` (or `(,) x y`), `Left x`, `Right y`, `Z`, `S`, `[]`, `(x : xs)` (or `(:) x xs`), `Char n`.
  * The parentheses around the cons constructor are not optional.
  * `Char` takes a natural number and turns it into a character.
* Pattern matchers: `{ Left x -> e ; Right y -> f }`
  * Pattern matchers can be applied like functions, e.g. `{ Z -> x, S -> y } 10` reduces to `y`.
  * Patterns must use the regular form of the constructor, e.g. `(x : xs)` and not `((:) x xs)`.
  * There are no nested patterns or default patterns.
  * Incomplete pattern matches will crash the interpreter.
* Literals: `1234`, `[e, f, g, h]`, `'a`, `"abc"`
  * Strings are represented as lists of characters.

## Call/CC
This interpreter has preliminary support for
[the call-with-current-continuation control flow operator](https://en.wikipedia.org/wiki/Call-with-current-continuation).
However, it has not been thoroughly tested.

To use it, simply apply the variable `callcc` like you would a function, e.g. `(callcc (\k. ...))`.

Continuations are printed as `λ!. ... ! ...`, like a lambda abstraction
with an argument named `!` which is used exactly once;
however, continuations are *not* the same as lambda abstractions
because they perform the side effect of modifying the current continuation,
and this is *not* valid syntax you can input into the REPL.

## Example code
The fixpoint function:
```
(\x. x x) \fix f x. f (fix fix f) x
```

Create a list by iterating `f` `n` times:
```
fix \iterate f x. { Z -> x ; S n -> iterate f (f x) n }
```

Create a list whose first element is `n - 1`, counting down to a last element of `0`:
```
\n. { (n, x) -> x } (iterate { (n, x) -> (S n, (n : x)) } (0, []) n)
```

Putting it all together to count down from 10:
```
>> let fix = (\x. x x) \fix f x. f (fix fix f) x; iterate = fix \iterate f x. { Z -> x ; S n -> iterate f (f x) n }; countDownFrom = \n. { (n, x) -> x } (iterate { (n, x) -> (S n, (n : x)) } (0, []) n) in countDownFrom 10
[9, 8, 7, 6, 5, 4, 3, 2, 1, 0]
```

Append two lists together:
```
fix \append xs ys. { [] -> ys ; (x : xs) -> (x : append xs ys) } xs
```

Reverse a list:
```
fix \reverse. { [] -> [] ; (x : xs) -> append (reverse xs) [x] }
```

Putting them together so we can reverse `"reverse"`:
```
>> let fix = (\x. x x) \fix f x. f (fix fix f) x; append = fix \append xs ys. { [] -> ys ; (x : xs) -> (x : append xs ys) } xs; reverse = fix \reverse. { [] -> [] ; (x : xs) -> append (reverse xs) [x] } in reverse "reverse"
"esrever"
```

Calculating `3 + 2` with the help of Church-encoded numerals:
```
>> let Sf = \n f x. f (n f x); plus = \x. x Sf in plus (\f x. f (f (f x))) (\f x. f (f x)) S Z
5
```

This expression would loop forever, but `callcc` saves the day!
```
>> y (callcc \k. (\x. (\x. x x) (\x. x x)) (k z))
y z
```

A few other weird expressions:
```
>> let D = \x. x x; F = \f. f (f y) in D (F \x. x)
y y
>> let T = \f x. f (f x) in (\f x. T (T (T (T T))) f x) (\x. x) y
y
>> (\x y z. x y) y
λy' z. y y'
>> { Char c -> Char (S c) } 'a
'b
```
