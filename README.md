# James Martin's Lambda Calculus
This project is a tool for me to learn about implementing various concepts from programming language theory, in particular those that relate to the lambda calculus.
I also hope to equip it with enough useful features that it is a usable for real programming tasks, at least as a toy.
Ideally, this will also be a useful tool for *others* to learn about the lambda calculus through experimentation.

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

## Roadmap
### Complete
* Type systems:
  * Untyped
* Representations:
  * The syntax tree
  * Reverse de Bruijn
* Syntax:
  * Basic syntax
  * Let expressions
* Evaluation strategies:
  * Lazy (call-by-name to normal form)

### Planned
Not all of these will necessarily (or even probably) be implemented.
This is more-or-less a wishlist of things I'd like to try to implement some day.
* Built-ins:
  * Integers
  * Characters
  * Strings
  * Lists
* Type systems:
  * all of the systems of the Lambda Cube
  * Hindley-Milner
  * and the calculus of (co)inductive constructions
    * and something based on cubical TT
    * and something with universe polymorphism
    * and something with insanely dependent types
    * and support for tactics
  * and something with non-trivial subtyping
  * and something with row polymorphism
  * and something with typeclasses/constraints
  * and something with irrelevance (runtime, true irrelevance, prop)
  * and something with iso/equirecursive types?
  * (classical?) linear types
    * something with lifetimes, like Rust
    * something that would work on a quantum computer, at least in theory
    * something with proof nets?
* Macros, fexprs
* (Delimited) continuations
  * Something based on lambda-mu?
* Effects:
  * A (co)effects system.
  * Call-by-push-value.
* Representations:
  * A more conservative syntax tree that would allow for better error messages
* Evaluation strategies:
  * The evaluation strategies documented by Thierry(?)
    * Full laziness
    * Complete laziness
  * Optimal
* Syntax:
  * Top-level definitions
  * Type annotations
  * `let*`, `letrec`
  * Pretty-printing mode.
  * Indentation-based syntax.
* Features:
  * A better REPL (e.g. the ability to edit the line buffer)
  * The ability to import external files
    * A good module system?
  * The ability to choose the type system or evaluation strategy
  * Better error messages for parsing and typechecking
  * Reduction stepping
