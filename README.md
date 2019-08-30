# James Martin's Lambda Calculus
An implementation of various type systems and evaluation strategies
for the lambda calculus.

This project is a work-in-progress, and currently lacks many essential features
that would be necessary to be a useful programming language.

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
  
### In-progress
* Type systems:
  * Simply typed
* Representations:
  * De Bruijn

### Planned
My ultimate goal is to develop this into a programming language
that would at least theoretically be practically useful.
I intend to do a lot more than this in the long run,
but it's far enough off that I haven't nailed down the specifics yet.

* Built-ins:
  * Integers
* Type systems:
  * Hindley-Milner
  * System F
* Representations:
  * A more conservative syntax tree that would allow for better error messages
* Evaluation strategies:
  * Complete laziness
  * Optimal
* Syntax:
  * Top-level definitions
  * Type annotations
  * `let*`, `letrec`
  * More syntax (parsing and printing) options:
    * Also allow warnings instead of errors on disabled syntax.
      * Or set a preferred printing style without warnings.
      * Or print in an entirely different syntax than the input!
    * Disable empty `application`: `()` no longer parses (as `\x. x`).
      * Forbid single-term `application`: `(x)` no longer parses as `x`.
    * Disable empty `variable-list`: `λ. x` no longer parses (as just `x`).
    * Disable block arguments: `f λx. x` is no longer permitted; `f (λx. x)` must be used instead.
      * Except for at the top level, where an unclosed lambda is always permitted.
    * Configurable `variable-list` syntax:
      * Mathematics style: One-letter variable names, no variable separators.
      * Computer science style: Variable names separated by commas instead of spaces.
    * Configurable `λ` syntax: any one of `λ`, `\`, or `^`, as I've seen all three in use.
      * Currently, either `λ` or `\` is permitted, and it is impossible to disable either.
    * Disable `let` expressions.
    * Disable syntactic sugar entirely (never drop parentheses).
    * Pedantic mode: forbid using more parentheses than necessary.
    * Pedantic whitespace (e.g. forbid ` (  a    b c)`).
  * Pretty-printing mode.
  * Indentation-based syntax.
* Features:
  * A better REPL (e.g. the ability to edit the line buffer)
  * The ability to import external files
  * The ability to choose the type system or evaluation strategy
  * Better error messages for parsing and typechecking
  * Reduction stepping
