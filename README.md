# The Ivo Programming Language
Ivo (IPA: /aɪvoʊ/) is a programming language intended
as a tool for its author to explore interesting programming language features.

Ivo is currently in a very early stage of development
and most likely is not of any practical or academic interest;
however, that may change in the future.
This README serves to document the language as it currently stands,
not what the language one day hopes to be.

## Using the Ivo interpreter
You may run the Ivo interpreter (`ivo`)
by installing it to your local path using `stack install`,
or equivalently, using `stack run [-- args...]`.
For information about `ivo`'s command line arguments, please refer to `ivo --help`
(or `stack run -- --help`).

Type in your command, definition, or expression at the prompt: `>> `.
Expressions will be typechecked, evaluated using call-by-value, and then printed.

Exit the prompt with `Ctrl-d` (or equivalent).

### Interpreter commands
These commands are available:

* `:clear`: Clear all of your variable definitions.

* `:load <filename>`:

  Execute a file containing Ivo definitions and expressions in the interpreter.
  Variables already defined in the interpreter will be defined in the file;
  variables defined by the file will be defined in the interpreter.

  The filename may contain spaces, but trailing whitespace will be trimmed.

* `:printTypes <both/decls/exprs/off>`:

  Print to STDERR the inferred types of top-level declarations,
  of expressions entered into the interpreters,
  of both, or of neither.

  This setting defaults to `off`.

* `:trace <off/local/global>`:

  * If the argument is `local`, intermediate expressions will be printed
    as they are evaluated;

  * If the argument is `global`, the *entire* expression will be printed
    with each evaluation step.

  * The default value is `off`.

## The Ivo language
### Syntax
The parser's error messages currently are virtually useless, so be very careful with your syntax.

* Variable names: any sequence of letters.
* Function application: `f x y`
* Lambda abstraction: `\x y z. E` or `λx y z. E`
* Let expressions: `let x = E; y = F in G`
  * The definitions of let expessions may be recursive:
    `let undefined = undefined in undefined`.
* Parenthetical expressions: `(E)`
* Constructors: `()`, `(x, y)` (or `(,) x y`), `Left x`, `Right y`, `Z`, `S`, `[]`, `(x :: xs)` (or `(:) x xs`), `Char n`.
  * The parentheses around the cons constructor are not optional.
  * `Char` takes a natural number and turns it into a character.
* Pattern matchers: `{ Left a -> e ; Right y -> f }`
  * Pattern matchers can be applied like functions, e.g. `{ Z -> x, S -> y } 10` reduces to `y`.
  * Patterns must use the regular form of the constructor, e.g. `(x :: xs)` and not `((::) x xs)`.
  * There are no nested patterns or default patterns.
  * Incomplete pattern matches will crash the interpreter.
* Literals: `1234`, `[e, f, g, h]`, `'a`, `"abc"`
  * Strings are represented as lists of characters.
* Type annotations: there are no type annotations; types are inferred only.
* Comments: `// line comment`, `/* block comment */`

Top-level contexts (e.g. the REPL or a source code file)
allow declarations (`let x = E` without multiple definitions `in ...`),
which make your definitions available for the rest of the program's execution.
You must separate your declarations and expressions with `;`.

### Types
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

### Builtins
Builtins are variables that correspond with a built-in language feature
that cannot be replicated by user-written code.
They still are just variables though; they do not receive special syntactic treatment.

* `callcc : ∀a b. (((a -> b) -> a) -> a)`:
  [the call-with-current-continuation control flow operator](https://en.wikipedia.org/wiki/Call-with-current-continuation).

Continuations are printed as `λ!. ... ! ...`, like a lambda abstraction
with an argument named `!` which is used exactly once;
however, continuations are *not* the same as lambda abstractions
because they perform the side effect of modifying the current continuation,
and this is *not* valid syntax you can enter into the REPL.

### Example code
You can see some example code in `examples/examples.ivo`.
