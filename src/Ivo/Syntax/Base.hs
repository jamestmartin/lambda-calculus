-- | The language's abstract syntax.
--
-- The abstract syntax is very forward-thinking
-- in that it's designed to accomodate for features
-- which haven't been implemented yet,
-- partly to make sure that I won't need to change it in a way
-- that will involve rewriting lots of other code when I do implement those features.
-- Plus, this serves as documentation for what the syntax /will/
-- be and why I designed it to be the way it is.
--
-- That means that not all of this syntax is necessarily supported by the parser,
-- and that even if it /is/, that the associated feature isn't necessarily
-- implemented by the typechecker, interpreter, or compiler.
module Ivo.Syntax.Base
  ( Text, NonEmpty (..)
  , Scope (..), TopLevel (..), Definition (..), PublicOrPrivate (..)
  , Kind, Type, Expr (..), Sig (..), Sig' (..), Lit (..)
  , Pattern (..), Guard, CaseBranch (..)
  , FlexibleSeparatorSyntax
  ) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)

newtype Scope = Scope [TopLevel]
  deriving Show

data TopLevel
  -- | Bring the definitions in a module into scope.
  --
  -- > open Module1
  --
  -- An explicit import list is also permitted:
  --
  -- > open Module2 ( foo ; bar ; baz )
  --
  -- All of the imports can be re-exported using @pub open@,
  -- or individual imports may be made public
  -- by individually prefixing each with @pub@.
  -- These syntaxes may not be combined.
  --
  -- > pub open Module1 ;
  -- > open Module2 ( pub foo ; bar ; pub baz ) ;
  --
  -- Fields can additionally be renamed or hidden:
  --
  -- > open Module3 using ( foo ; bar ) renaming ( foo → baz ) ;
  -- > open Module4 hiding ( foo ) ;
  -- > open Module5 hiding ( foo ) renaming ( bar → baz ) ;
  --
  -- Combining an explicit import list with a hiding list is forbidden,
  -- seeing as doing so would be completely pointless.
  --
  -- It is also possible to open signatures, but only into other signatures.
  -- When opening into a signature, all declarations are added to
  -- the enclosing signature's declarations /regardless/ of
  -- whether the open was marked public,
  -- but default definitions are imported only as part of a @pub open@.
  --
  -- Note that this feature may be used as part of this idiom for extending a module:
  --
  -- > Λ m → sig Monoid {
  -- >     pub open Semigroup m ;
  -- >     ∅ : m ;
  -- >     // ... identity laws ---
  -- > }
  --
  -- Data constructor patterns are exported
  -- separately from the constructors themselves.
  -- The patterns are exported under the type constructor's name;
  -- the constructors are exported like functions.
  -- Exporting patterns implicitly exports the constructors as well,
  -- but the constructors can subsequently be hidden.
  -- Here's what I mean:
  --
  -- > // export both constructors and patterns:
  -- > open data <sig> using ( MyType ( Con1 ; Con2 ) ) ;
  -- > // export just constructors, not patterns:
  -- > open data <sig> using ( MyType ; Con1 ; Con2 ) ;
  -- > // export just patterns, not constructors:
  -- > open data <sig> using ( MyType ( Con1 ; Con2 ) ) hiding ( Con1 ; Con2 ) ;
  -- > // export the patterns and data constructors, but not the type constructor:
  -- > open data <sig> using ( MyType ( Con1 ; Con2 ) ) hiding ( MyType ) ;
  --
  -- Combined with default definitions in the signature
  -- (to whom both the constructors and patterns are /always/ in scope)
  -- this can be used to implement opaque data types
  -- with smart constructors and/or eliminators,
  -- because the type could only be constructed via exported default functions,
  -- but pattern matched against as usual (or vice versa, or both).
  --
  -- An "import all" syntax is also supported:
  --
  -- // import all constructors
  -- > open data <sig> using ( MyType ( .. ) ) ;
  -- // hide all constructors
  -- > open data <sig> hiding ( MyType ( .. ) ) ;
  --
  -- This uses 'FlexibleSeparatorSyntax'.
  = Open PublicOrPrivate Expr [(Text, Maybe (Maybe [Text]))] -- * @using@
                              [(Text, Maybe (Maybe [Text]))] -- * @hiding@
                              [(Text, Text)]                 -- * @renaming@
  -- | See 'Definition'.
  --
  -- A definition may be exported from the enclosing module or signature
  -- by prefixing it with the keyword @pub@:
  --
  -- > pub foo : bar = baz
  | Define PublicOrPrivate Definition
  -- | At the top level, @data <sig>@ is syntactic sugar for @open data <sig>@.
  --
  -- See 'Open' and 'Data' for more information..
  | TLData Expr
  -- | At the top level, @axiom <sig>@ is syntactic sugar for @open axiom <sig>@.
  --
  -- See 'Open' and 'Axiom' for more information.
  | TLAxiom Expr
  -- | At the top level, @adt <...>@, @gadt <...>@, and @sig <...>@
  -- are syntactic sugar for @open data adt <...>@, @open data gadt <...>@,
  -- and @open data sig <...>@ respectively.
  --
  -- See 'Open', 'Data', and 'Sig' for more information.
  | TLSig Sig
  deriving Show

-- | 'Public' is written with @pub@;
-- 'Private' indicates the abscence of an access modifier.
data PublicOrPrivate = Public | Private
  deriving Show

-- | Define a name as an expression, with an optional type annotation.
--
-- > foo : type = <expression> ; // with type annotation
-- > bar        = <expression> ; // with inferred type
data Definition = Definition Text (Maybe Type) Expr
  deriving Show

-- | An expression which represents the type of a type.
type Kind = Type
  deriving Show
-- | An expression which represents the type of a value.
type Type = Expr
  deriving Show

data Expr
  ---
  --- Expressions pertaining to signatures and modules
  ---

  -- | Automatically implement a signature by creating (co)inductive data types
  -- with the given constructors/eliminators, and additionally pattern matching.
  --
  -- > data <signature expression>
  --
  -- See 'ADT', 'GADT', and 'Sig' for examples of the data types this can implement.
  --
  -- @data@ can be used to implement things with kind @Type → ... → Sig@
  -- by turning all type arguments into signature parameters,
  -- but generally speaking, the use of signature parameters should be preferred
  -- see 'Sig' (the type, not the constructor).
  = Data Expr
  -- | Automatically "implement" a signature by taking all of its definitions as axioms.
  --
  -- > axiom <signature expression>
  --
  -- Axiom "implementations" do not have any computational meaning
  -- and can only be typechecked, but not executed.
  -- It may be possible to execute a program which contains axioms,
  -- but the program will crash when the axioms are encountered.
  --
  -- Axioms are useful when writing proofs which you are not interested in executing,
  -- or as placeholders for functions you haven't implemented yet.
  | Axiom Expr
  -- | A signature literal. See 'ADT', 'GADT', and 'Sig' for examples.
  | SigE Sig
  -- | A module expression with an optional signature annotation.
  -- Like with signatures, modules may have type parameters.
  --
  -- If a module has a signature annotation,
  -- then only the names in the signature will be exported
  -- (and /all/ of those names will, even if they were not marked public
  -- at their definition site).
  -- All names exposed by the signature will be typechecked
  -- against their types in the signature.
  --
  -- If no signature is specified, then the module will have an anonymous signature
  -- based on the inferred (or specified) types for all of its public definitions.
  --
  -- > Λ a → mod : Monoid (List a) {
  -- >     ∅    = [] ;
  -- >     _<>_ = /* don't feel like writing this code right now */ ;
  -- >
  -- >    monoid_id_left = ... ;
  -- >    ...
  -- > }
  --
  -- Like with most scopes, it is possible to open a module within another module;
  -- public imports will be re-exported if specified by the module's signature.
  --
  -- A public definition which is not included in the module's signature
  -- will issue a warning, but not fail to compile.
  -- Because definitions are exported if and only if they are part of the signature,
  -- @pub@ is always redundant in a module with a type signature.
  --
  -- This uses 'FlexibleSeparatorSyntax'.
  | Mod [Either (NonEmpty Text, Kind) Text] (Maybe Type) [TopLevel]
  -- | A field accessor for signatures and modules.
  --
  -- A signature @sig : Sig@ which defines @foo : bar@
  -- and a module implementing that signature @mod : sig@, then:
  --
  -- > sig::foo : sig → bar
  -- > mod::foo :       bar
  | Access Expr Text

  ---
  --- Expressions pertaining to types
  ---

  -- | Universal type quantification with optional kind annotations.
  --
  -- With unicode syntax:
  --
  -- > ∀ foo ( bar : baz ) quux → type
  --
  -- With ASCII syntax:
  --
  -- > forall foo ( bar : baz ) quux -> type
  --
  -- Forall is constructed by 'TypeLam' and eliminated by 'TypeApp';
  -- however, occurrences of 'TypeLam' and 'TypeApp'
  -- will usually be inferred and do not need to be written.
  --
  -- 'Forall' is used for abstracting over /computationally-irrelevant/ terms;
  -- the terms are usually dependent and invisible, but /may/ not be.
  --
  -- 'Arrow' is used for abstracting over /computationally-relevant/ terms;
  -- the terms are always visible and usually independent, but /may/ be.
  --
  -- The distinction between these quantifiers was first made known to me
  -- by Sam Lindley and Conor McBride's Hasochism paper.
  | Forall (NonEmpty (Either (NonEmpty Text, Kind) Text)) Type
  -- | Function types with optional argument names;
  -- multiple arguments may be bound with one type.
  --
  -- With unicode syntax:
  --
  -- > ( foo bar : baz ) → quux → fwee
  --
  -- With ASCII syntax:
  --
  -- > ( foo bar : baz ) -> quux -> fwee
  --
  -- Arguments @foo@ and @bar@ both have type @baz@;
  -- the argument with type @quux@ does not have a name;
  -- @fwee@ is the function's return type.
  --
  -- Note that an empty list of argument names
  -- corresponds with @foo → bar@, /not/ @(: foo) → bar@,
  -- which is syntactically invalid.
  --
  -- 'Arrow' is constructed by 'Lam' and eliminated by 'App'.
  --
  -- 'Arrow', as a relevant quantifier, contrasts 'Forall', an irrelevant quantifier;
  -- more information in the documentation for 'Forall'.
  | Arrow [Text] Type Type
  -- | Type annotations (or kind annotations; they are the same thing).
  --
  -- > foo : bar
  | Ann Expr Type
  -- | A hole represents a type or value to be inferred.
  -- If inference fails, then the inferred type or kind
  -- and detailed information about the surrounding context will be printed as an error,
  -- to be used as a tool for type-driven development.
  --
  -- > _
  | Hole

  ---
  --- Expressions pertaining to values
  ---

  -- | The constructor for 'Forall',
  -- which explicitly binds a forall-quantified type variable
  -- as a value-level variable with an optional kind annotation.
  --
  -- With unicode syntax:
  --
  -- > Λ   a       → foo : ∀   a       → bar
  -- > Λ ( a : k ) → foo : ∀ ( a : k ) → bar
  --
  -- With ASCII syntax:
  --
  -- > ^   a       -> foo : forall   a       -> bar
  -- > ^ ( a : k ) -> foo : forall ( a : k ) -> bar
  --
  -- The name of the variable and the type and in the value
  -- do not have to be the same, and in these specific examples,
  -- there is no need whatsoever to annotate the kind in both
  -- the type and value; if one is specified, the other will
  -- /always/ be able to be inferred.
  --
  -- These variables still are type variables and do not contain data;
  -- they will be completely erased at runtime so the computational behavior
  -- of a function cannot depend on them.
  --
  -- Type lambdas are usually inferred and do not need to be written down.
  -- The purpose of type lambdas is to bring type variables into scope
  -- for use by type annotations and type applications.
  --
  -- In the presence of inferred type lambdas,
  -- the explicit type lambda will always bind the /innermost/ forall.
  -- There are a few reasons for this:
  --
  -- 1. Type quantifiers can only depend on types to their left,
  --    meaning that the innermost quantifiers tend to be the most complex
  --    and most difficult to infer.
  --
  -- 2. If the kind of a quantified type is inferred to be polymorphic,
  --    a new kind variable will be introduced and quantified
  --    on the /left/ for the reasons described in (1).
  --    These implicitly-quantified variables /cannot/
  --    be bound by type lambdas or applied using type applications,
  --    but even if they could be, binding quantifiers inner-to-outer
  --    would prevent this transformation from affecting
  --    the meaning of the expression.
  --
  -- 3. Module parameters may be given specific values
  --    /or/ be made polymorphic, depending on how the module is used.
  --    These inserted foralls are inserted (obviously) on the /left/.
  | TypeLam (NonEmpty (Either (NonEmpty Text, Kind) Text)) Expr
  -- | Type application, the eliminator for 'Forall'.
  --
  -- Suppose you have an expression @foo : forall a → bar a@,
  -- but the type @a@ is ambiguous and cannot be inferred.
  -- You may explicitly instantiate @foo@ with @Nat@:
  --
  -- > foo @ Nat : bar Nat
  --
  -- Type application can usually be inferred without needing to be specified.
  | TypeApp Expr Type
  -- | An lambda abstraction, the constructor for 'Arrow'.
  --
  -- With unicode syntax, if @baz : quux@:
  --
  -- > λ   foo         → baz : bar → quux
  -- > λ ( foo : bar ) → baz : bar → quux
  --
  -- With ASCII syntax:
  --
  -- > λ   foo         -> baz
  -- > λ ( foo : bar ) -> baz
  --
  -- A lambda expression may pattern match against its arguments:
  --
  -- > λ   _        → foo   // the argument is irrelevant
  -- > λ   x    y   → foo   // multiple arguments
  -- > λ ( x  , y ) → foo   // pattern match against a *-pair (data)
  -- > λ ( π₂ → y ) → foo   // project the first argument of an &-pair (codata)
  --
  -- As a special case, a lambda which matches an impossible pattern
  -- may have no body:
  --
  -- > λ !   // that's really the whole thing!
  --
  -- Even more generally, it may branch based on the pattern match,
  -- after binding any number of arguments which it doesn't branch on,
  -- which will be shared across every case branch.
  --
  -- > λ (x , y) {
  -- >     S n | Left  m → f n m ;
  -- >       Z | Right z → x z ;
  -- >       _ |       _ → y ;
  -- > }
  --
  -- Pattern matching is a very complex syntactic construct
  -- which can do even more than just this;
  -- see 'CaseBranch' for the full detail of what it can do.
  --
  -- Although the 'Lam' constructor allows for either
  -- the list of patterns (for non-branching arguments)
  -- or the list of case branches (for branching arguments)
  -- to be empty, /at least one/ must consume an argument;
  -- both may not be empty.
  | Lam      [Pattern] [CaseBranch]
  -- | A lambda with only one, trivial branch, e.g. @λ x y z -> e@.
  --
  -- See 'Lam'.
  | LamBlock [Pattern]  Expr
  -- | Function application, the eliminator for 'Arrow'.
  -- (See also 'AppI', mixfix function application.)
  --
  -- Syntactically, function application is represented by juxtiposition.
  --
  -- If @foo : baz -> quux@ and @bar : baz@:
  --
  -- > foo bar : quux
  | App Expr (NonEmpty Expr)
  -- | Mixfix function application (see also 'App').
  --
  -- A mixfix operator name is composed of text
  -- plus underscores where the arguments go.
  -- Here, we replace the underscores with a space
  -- corresponding with each argument in the list,
  -- and, in the case of a partially applied mixfix operator,
  -- leave the underscores for each argument /not/ applied.
  --
  -- Here's a fully applied example for @if_then_else_@:
  --
  -- > if foo then bar else baz
  --
  -- This is a single infix application: @AppI "if then else " [foo, bar, baz]@.
  --
  -- On the other hand, if we only provide the @then@ case:
  --
  -- > if_then bar else_
  --
  -- This is represented as @AppI "if_then else_" [bar]@.
  --
  -- If a mixfix operator is left completely unapplied, @if_then_else_@,
  -- then it should be parsed as a regular variable name
  -- and applied using regular function application.
  | AppI Text (NonEmpty Expr)
  -- | A let expression, used to define variables in a local scope.
  --
  -- The definitions in a let expression can include both variable definitions
  -- and /opening modules/ (and consequently, even local data types).
  --
  -- Defining two variables:
  --
  -- > let foo = bar ; baz : quuz = splee in fwap
  --
  -- Opening a module and defining a variable:
  --
  -- > let open Module ; foo = bar in baz
  --
  -- The definitions in a let expression may be recursive, interdependent,
  -- or even mutually recursive, just like top-level declarations.
  -- (However, the use of these features may interfere with type inference.)
  -- Variables bound in a let expression may shadow variables,
  -- including those defined by /other/ let expressions,
  -- but a variable can only be defined at most once in a single let expression.
  --
  -- This uses 'FlexibleSeparatorSyntax'.
  | Let [TopLevel] Expr
  -- | A case expression, used to pattern match against some variables.
  --
  -- Multiple expressions to be matched against are separated by bars (@|@)
  -- with 'FlexibleSeparatorSyntax'.
  --
  -- > case foo | bar {
  -- >     π₁ ( Left  x | _ ) → x ;
  -- >     π₁ ( Right _ | z ) → z ;
  -- >     π₂ ( Right y | _ ) → y ;
  -- >     π₂ ( Left  _ | z ) → z ;
  -- > }
  --
  -- A case expression with no arguments may be used just for guards:
  --
  -- > case {
  -- >     ? cond1 → x ;
  -- >     ? cond2 → y ;
  -- >     ? else  → z ;
  -- > }
  --
  -- 'Lam' may also perform case branching; lambda case syntax
  -- should generally be preferred over binding a variable with lambda
  -- and then immediately 'Case'-ing it.
  --
  -- For more information about the (very complex) syntax of case branches,
  -- see 'CaseBranch'.
  | Case      [Expr] [CaseBranch]
  -- | A case with only one, trivial branch, e.g.
  -- @case x | y of a , b | c , d → e@.
  --
  -- See 'Case'.
  | CaseBlock [Expr] [Pattern]    Expr
  -- | Integer, string, character, and list literals.
  | Lit (Lit Expr)
  -- | Variables.
  | Var Text
  deriving Show

-- | A signature literal.
--
-- Signatures are the type of modules.
-- They're basically record types,
-- associating a bunch of names with a bunch of types.
--
-- Signatures can be used for many things, including defining data types.
-- See 'Data', 'Axiom', and 'Mod' for a few ways they can be used.
--
-- However, there are multiple syntaxes for writing them,
-- which is (obviously) what this type enumerates.
--
-- Do not confuse type functions which return a signature (@Λ a → <sig>@),
-- signature parameters (@sig a { ... }@),
-- and type indices (@sig { Foo : a → Type }@)!
-- These three features serve entirely different purposes.
--
-- Type functions which return a signature /are not signatures per se/
-- and /must/ be instantiated with a type argument to become a signature.
-- The created signature is /not/ polymorphic over the bound type.
--
-- For example, this signature describes for any type @m@ that @m@ is a semigroup:
--
-- > Λ m → sig {
-- >     _<>_ : m → m → m ;
-- >     <>IsAssoc : ( x y z : m ) → ( x <> y ) <> z ≡ x <> ( y <> z ) ;
-- >
--
-- Signature parameters are universally quantified
-- across all definitions in the signature,
-- so that the signature may reference its own definitions with any type parameter,
-- and critically, the /implementation/ must be polymorphic over the
-- signature parameter as well.
--
-- The way quantifiers work is that they're added as /explicit/ parameters to kinds
-- and as /implicit/ parameters to types for all declarations and definitions
-- in the signature.
--
-- For example, this signature describes a list data type
-- which works for any type @a@, plus a @join@ operation:
--
-- > sig a {
-- >     List : Type ;
-- >     []   : List a ;
-- >     _∷_  : List a → List a → List a ;
-- >     join : List (List a) → List a ;
-- > }
--
-- Note that the @join@ operation would be illegal for @Λ a → sig { ... }@
-- because the signature's implementation doesn't have to be polymorphic in @a@,
-- but @join@ instantiates @a@ with @List a@.
--
-- It is the difference between describing a polymorphic type
-- and a family of monomorphic types which isn't necessarily universally implemented.
--
-- Finally, type indices are just the arguments to a type constructor
-- which are not quantified by @sig@ (but rather by each individual definition)
-- and thus are not necessarily parametric, e.g.:
--
-- > sig {
-- >     Fin : Nat → Type ;
-- >
-- >     FZ  :               Fin     Z   ;
-- >     FS  : ∀ n → Fin n → Fin ( S n ) ;
-- > }
data Sig
  -- | Every kind of signature literal allows
  -- for type parameters with optional kind signatures,
  -- so those have been factored out into this constructor.
  = SigQs [Either (NonEmpty Text, Kind) Text] Sig'
  deriving Show
-- | A signature literal variant, excluding type parameters.
--
-- ('SigQs' is the constructor with the type parameters.)
data Sig'
  -- | An algebraic data type (ADT) signature. @False@ for data, @True@ for codata.
  --
  -- > adt Nat { Z + S Nat }
  -- > adt Stream a { Head a & Tail ( Stream a ) }
  --
  -- Preceding @+@s and @&@s are also permitted, which looks like this:
  --
  -- The syntax @+@ and @&@ were chosen because the branches of ADTs correspond with
  -- additive disjunction (written @⊕@ or @+@) and additive conjunction (written @&@)
  -- from linear logic.
  --
  -- (The arguments correspond with multiplicative conjunction (@⊗@ or @*@),
  -- and multiplicative disjunction (@⅋@ or @|@) respectively;
  -- however, these arguments are written using mere juxtaposition
  -- instead of using an explicit syntax because it is unambiguous.)
  --
  -- This uses 'FlexibleSeparatorSyntax', e.g.:
  --
  -- > adt List a {
  -- >     + []
  -- >     + _∷_ a ( List a )
  -- > }
  = ADT Bool Text (Maybe Kind) [(Text, [Type])]
  -- | A generalized algebraic data type (GADT) signature.
  --
  -- GADTs are similar to ADTs. However, thanks to their extended syntax,
  -- they also may be non-parametric in their type arguments
  -- (non-parametric arguments, i.e. indices, are specified in the kind signature),
  -- or be used to define higher-inductive types.
  --
  -- `Vec a n` has a type parameter `a` and a type index `n`:
  --
  -- > gadt Vec a : Nat → Type {
  -- >     []  :                   Vec a     Z   ;
  -- >     _∷_ : ∀n. a → Vec a n → Vec a ( S n ) ;
  --
  -- `Multiset a` is a higher-inductive type with a commutative `union` constructor.
  --
  -- > gadt Multiset a {
  -- >     empty     :                 Set a ;
  -- >     singleton :             a → Set a ;
  -- >     _union_   : Set a → Set a → Set a ;
  -- >
  -- >     // An additional constructor for Multiset's *identity* type.
  -- >     union_commutative : ( xs ys : Set a ) → xs union ys ≡ ys union xs;
  -- > }
  --
  -- This uses 'FlexibleSeparatorSyntax'.
  | GADT Text (Maybe Kind) [(NonEmpty Text, Type)]
  -- | The most general syntax for signatures.
  --
  -- General signatures are simply collections of names and their types.
  -- They are not necessarily the template for a /data/ type;
  -- they may also be used for defining typeclasses,
  -- an abstract interface for multiple implementations of a data structure,
  -- a set of definitions to be taken as axiomatic, etc.
  --
  -- However, general signatures still /can/ be used to define data types
  -- which are potentially more general than even GADTs,
  -- for example mutually recursive types or inductive-inductive types.
  --
  -- A signature which describes the Monoid typeclass:
  --
  -- > sig m {
  -- >     empty : m ;
  -- >     _<>_  : m -> m -> m ;
  -- >
  -- >     monoid_id_left  : ( y : m ) → empty <> y ≡ y ;
  -- >     monoid_id_right : ( x : m ) → x <> empty ≡ x ;
  -- >     monoid_assoc    : ( x y z : m ) → x <> (y <> z) ≡ (x <> y) <> z ;
  -- > }
  --
  -- A signature which describes an axiom, the law of the excluded middle:
  --
  -- > sig a { lem : ¬ a + a }
  --
  -- The simultaneous definition of two mutually-recursive data types:
  --
  -- > sig {
  -- >     Even , Odd : Nat → Type ;
  -- >
  -- >     zeroIsEven    :              Even    Z    ;
  -- >     succOddIsEven : ∀n. Odd  n → Even ( S n ) ;
  -- >     succEvenIsOdd : ∀n. Even n → Odd  ( S n ) ;
  -- > }
  --
  -- (Multiple comma-separated declarations use 'FlexibleSeparatorSyntax'.)
  --
  -- An inductive-recursive pair of types:
  --
  -- > // An inductive-recursive pair of types:
  -- > open data sig ( a : Type ) ( _#_ : a → a → Type ) {
  -- >     // A list in which all elements are distinct from each other
  -- >     // by the distinctness relation `_#_`.
  -- >     Dlist : Type ;
  -- >     // A proof that the first argument is distinct
  -- >     // from all of the elements of the second argument.
  -- >     Fresh : a → Dlist a _#_ → Type ;
  -- >
  -- >     nil  :                                                       Dlist a _#_ ;
  -- >     cons : ( x : a ) → ( xs : Dlist a _#_ ) → Fresh a _#_ x xs → Dlist a _#_ ;
  -- >
  -- >     freshNil  : ( x : a ) → Fresh a _#_ x nil ;
  -- >     freshCons : ( x : a )
  -- >               // the original list
  -- >               → ( head : a ) → ( tail : Dlist a _#_ ) → ( p : Fresh a _#_ head tail )
  -- >               // `x` is distinct from both the head and tail of the original list
  -- >               → x # head -> Fresh a _#_ x tail
  -- >               // thus `x` is distinct from the /entire/ original list.
  -- >               → Fresh a _#_ x ( cons head tail p ) ;
  -- > }
  --
  -- Adding yet more complexity, signatures may also contain a default implementation,
  -- which is a definition, rather than a type declaration.
  -- Even further, the type of the default implementation may be a supertype of
  -- the declared type of the field, so that the default implementation
  -- may only be used if the definitions of other fields
  -- allow the default implementation's type to unify with the declared type.
  --
  -- If the default implementation's type is identical to the declared type,
  -- then it is sufficient to only include the default implementation's definition;
  -- that the field is part of the signature is implied by its definition.
  --
  -- For typechecking purposes, the equality of signatures
  -- is based only on the declared types of fields
  -- (including fields implicitly declared by a definition)
  -- and the definitions themselves are completely ignored;
  -- for the purposes of implementing a module,
  -- the default definitions will be exported as the implementations
  -- of their respective declared fields in the signature unless overridden
  -- by the module implementation.
  --
  -- Examples of default implementations:
  --
  -- > sig {
  -- >     Fwip :              Type         ;
  -- >     foo  :              a            ; // no default implementation
  -- >     bar  :              b     = x    ; // `bar` defaults to `x`
  -- >     baz  :              Thing        ;
  -- >     // `baz` defaults to `cool` only if an instance of `Cool Thing` is in scope
  -- >     baz  : Cool Thing ⇒ Thing = cool ;
  -- > }
  --
  -- I don't feel like including examples of `open` in signatures
  -- because I'm sick of writing docs, so I hope you get the idea
  -- based on my wall of text.
  --
  -- Finally, this is how visibility works:
  --
  -- * All declarations are public; @pub@ modifiers are forbidden
  --   because they would be strictly redundant.
  --
  -- * All definitions associated with a declaration are public;
  --   @pub@ modifiers are allowed but redundant.
  --
  -- * Definitions without a declaration are public only if marked @pub@;
  --   otherwise, there will be no "inferred declaration".
  | Sig [Either (NonEmpty Text, Type) TopLevel]
  deriving Show

-- | A pattern matching against a data type's constructors
-- or a codata type's eliminators.
--
-- A nested pattern is composed of some depth of codata eliminators
-- containing some depth of data constructors;
-- matching codata eliminators is used to construct codata types,
-- whereas matching data constructors is used to eliminate data types.
--
-- Patterns are used primarily in the context of 'CaseBranch'es.
data Pattern
  -- | Bind the matched value to a variable, with an optional type annotation.
  --
  -- > foo : bar
  -- > foo
  --
  -- A variable name may be bound multiple times in the same pattern
  -- if and only if the type checker can prove that both
  -- uses have identicial values.
  = PatVar Text (Maybe Type)
  -- | An irrelevant pattern completely ignores the matched value.
  --
  -- > _
  | Irrelevant
  -- | An impossible pattern matches a value of a type which
  -- the typechecker can prove is not possibly inhabited.
  --
  -- > !
  | Impossible
  -- | Match against a data constructor or codata eliminator.
  -- (See also 'PatAppI'.)
  --
  -- > Left x
  -- > π₁ xy
  -- > Pair x y
  | PatApp Text [Pattern]
  -- | Match against a mixfix data constructor or codata eliminator.
  -- (See also 'PatApp'.)
  --
  -- The mixfix constructor must be completely applied to patterns.
  --
  -- > x ∷ xs    // Pattern match against a list constructor.
  | PatAppI Text (NonEmpty Pattern)
  -- | Apply a function to the value and then pattern match against it.
  -- This may be used with e.g. copattern eliminators.
  --
  -- If the matched value has type @a@, then @( f : a → b ) → ( x : b )@.
  --
  -- > π₁ → foo  // Pattern match against the left value of an &-pair.
  --
  -- This syntax is based on Haskell's view patterns.
  | View Expr Pattern
  | PatLit (Lit Pattern)
  deriving Show

-- | A case branch has a left-hand side which
-- consists of some number of patterns to be matched and guards,
-- and a right-hand side consisting of an expression.
-- In an expression with multiple cases branches,
-- each left-hand side has the same type,
-- and each right-hand side has the same type (plus copattern matches).
--
-- To give a bit of an example, in:
--
-- > {
-- >     pat1 | pat2 → foo ;
-- >     pat3 | pat4 → bar ;
-- > }
--
-- `pat1` and `pat3` both match a value of the same type,
-- `pat2` and `pat4` both match a value of the same different type,
-- and `foo` and `bar` both have yet another different matching type.
--
-- In the case of copattern matches, e.g.
--
-- > {
-- >     π₁ → foo ;
-- >     π₂ → bar ;
-- > }
--
-- then @foo : a@ and @bar : b@ have different types,
-- but the return type of the expression as a whole remains the same: @a & b@.
--
-- In every context, the case branch syntax uses 'FlexibleSeparatorSyntax'.
--
-- Additionally, if there is only one case branch,
-- all of the parentheses and semicolons can usually be elided
-- in a context where block syntax is permitted:
--
-- > <pat> → <expr>
--
-- The behavior of case branches is yet more complex: see 'Refinement' and 'Guards'
-- for the additional things that case branches can do.
data CaseBranch
  -- | A branch may introduce additional values to match against using __plus clauses__.
  -- This allows /refining/ previous branches, for example by introducing a proof
  -- so that two variables may be /unified/ by matching against the identity type,
  -- or by proving that a type is uninhabited so that an impossible pattern may be used.
  --
  -- For example, when matching against two values @x : Nat@, @y : Nat@
  -- such that @x ~ z × 2@ and @y ~ z + z@,
  -- and given a function @p : (z : Nat) → z × 2 ≡ z + z@:
  --
  -- > {
  -- >     // additionally pattern match against the value `p z`
  -- >     x y + p z
  -- >     // now that a proof of equality is in-scope, we can refine our match
  -- >     x x | Refl → ...
  -- > }
  --
  -- The additional patterns may be matched without modifying the original pattern;
  -- in this case, the patterns list will only
  -- include the patterns for newly-introduced matches,
  -- and the rest of the pattern match can be assumed to be the same.
  --
  -- > {
  -- >     some complex patterns | foo ;
  -- >     // `...` is identical to writing `some complex patterns`;
  -- >     // all of the variables that it bound are still in scope.
  -- >     ...                   | pat1 → asdasd
  -- >     ...                   | pat2 → iouqwe
  -- > }
  --
  -- This syntax is based on Agda's with clauses.
  = Refinement [Pattern] [Expr] (NonEmpty CaseBranch)
  -- | In addition to pattern matches, branches may be subdivided by 'Guard' clauses,
  -- which are conditional expressions.
  -- If no guard clause is true, then further case patterns will be tried.
  -- (This is different from the behavior of Haskell, which can only match one pattern,
  -- and will fail due to non-exhaustive patterns if none of the guards match.)
  --
  -- > {
  -- >     One x   ? isGood x → foo  ; // if `x` is good
  -- >     Two y   ? isBad  y → bar    // if `y` is bad
  -- >             ? else     → baz  ; // if `y` is /not/ bad
  -- >     Three z ? isFun  z → quux ; // if `z` is fun
  -- >     _                  → floo ; // if `x` is not good /or/ `z` is not fun
  -- > }
  --
  -- With regards to view patterns and guard fall-through,
  -- patterns and guard clauses will always be evaluated top-to-bottom,
  -- to the extent that the behavior of the program would be affected
  -- by the order of evaluation for guard clauses.
  --
  -- This syntax is based on Haskell's guard clauses.
  | Guards     [Pattern]        (NonEmpty (Guard, Expr))
  -- | A case branch without refinement or guards.
  | PlainCase  [Pattern]         Expr
  deriving Show

-- | A guard is a kind of syntax for conditionals used as part of a 'CaseBranch'.
--
-- A guard is a sequence of plain pattern matches (/no/ plus or guard clauses)
-- against expressions /or/ expressions with a truth value (e.g. booleans)¹.
-- A guard returns true only if all patterns match and all expressions are truthy.
--
-- For example:
--
-- > ? x ← foo | y ← bar | isNice x && isNaughty y
--
-- Or as a simpler example:
--
-- > ? isCool foo
--
-- ¹: I might add a typeclass for 'truthiness' to allow using guard clause syntax.
-- Or it'll just be hardcoded for use by booleans; I'm not sure.
type Guard = [Either (Pattern, Expr) Expr]

-- | Special syntactic support for a few very important types.
data Lit r
  -- | @12310@, @+1338848@, @-131@
  = LitInt  Integer
  -- | @'a@, @'#@, @'\n@,
  | LitChar Char
  -- | @"And so I said, \"this language SUCKS!\""@
  | LitStr  Text
  -- | @[ x ; y ; z ]@, @[ ]@
  --
  -- Note that to minimize the number of reserved characters,
  -- (so that e.g. @[]@ can be a constructor name and not just syntax)
  -- square brackets are /keywords/ and not /tokens/,
  -- so e.g. in @[x ; y]@, @[x@ and @y]@ are /identifiers/.
  -- You /must/ leave a space: @[ x ; y ]@.
  --
  -- This uses 'FlexibleSeparatorSyntax'.
  | LitList [r]
  deriving Show

-- | When the languages has things within brackets separated by a divider,
-- that divider will usually be allowed redundantly both preceding and trailing.
--
-- This allows for more flexible layouts, for example with 'CaseBranch':
--
-- > {
-- >     <pat> → <expr> ;
-- >     <pat> → <expr> ;
-- > }
-- >
-- > {
-- > ;   <pat> → <expr>
-- > ;   <pat> → <expr>
-- > }
-- >
-- > {   <pat> → <expr>
-- > ;   <pat> → <expr>
-- > }
--
-- The benefit of trailing or preceding dividers for multi-line things like this
-- is that it allows for uniformity, which looks good, and additionally,
-- it means that re-ordering elements is as simple as copy-pasting the
-- entire line, without having to add or remove dividers anywhere.
-- (Most programmers probably already know about this trick.)
--
-- This sort of syntax occurs in many places, e.g.:
-- 'ADT', 'GADT', 'Sig', 'Mod', 'CaseBranch', 'Use', and 'Let'.
--
-- This data type isn't actually used for anything;
-- it's just so that I can link to this explanation in Haddock.
data FlexibleSeparatorSyntax
