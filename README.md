# Lambda Calculus
This is a simple programming language derived from lambda calculus,
using the Hindley-Milner type system, plus some builtin types, `fix`, and `callcc`

## Usage
Run the program using `stack run` (or run the tests with `stack test`).

Type in your expression at the prompt: `>> `.
Yourexpression will be evaluated to normal form using the call-by-value evaluation strategy and then printed.

Exit the prompt with `Ctrl-d` (or equivalent).

## Commands
Instead of entering an expression in the REPL, you may enter a command.
These commands are available:

* `:load <filename>`: Execute a program in the interpreter, importing all definitions.
* `:clear`: Clear all of your variable definitions.
* `:check <on/off> <always/decls/off>`:
  * If the first argument is `off`, then expressions will be evaluated even if they do not typecheck.
  * If the second argument is `always`, inferred types will always be printed.
    If it is `decls`, then only declarations will have their inferred types printed.
    Otherwise, the type of expressions is never printed.
  * The default values are `on` `decls`.
* `:trace <off/local/global>`:
  * If the argument is `local`, intermediate expressions will be printed as the evaluator evaluates them.
  * If the argument is `global`, the *entire* expression will be printed each evaluator step.
  * The default value is `off`.

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
* Comments: `// line comment`, `/* block comment */`

Top-level contexts (e.g. the REPL or a source code file)
allow declarations (`let` expressions without `in ...`),
which make your definitions available for the rest of the program's execution.
You may separate multiple declarations and expressions with `;`.

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
You can see some example code in `examples.lc`.
