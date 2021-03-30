-- | The language's abstract syntax.
--
-- This source code does not document the concrete syntax in any detail;
-- please refer to the language reference for more information about that.
--
-- This abstract syntax may include features which have not been implemented yet.
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

newtype Scope = Scope [Definition]
  deriving Show

data Definition = Definition Visibility Item

-- | 'Public' is written with @pub@;
-- 'Private' indicates the abscence of an access modifier.
data Visibility = Public | Private

data Item
  -- | @<name> <patterns...> = <expression>@
  -- or @<name> <patterns...> <case branches>@
  = Def Text [Pattern] CaseBranches
  -- | @<names...> : <type>@
  | Decl (NonEmpty Text) Type
  -- | @open <expr> <using>? <hiding>? <renaming>?
  | Open Expr (Maybe OpenUsing) (Maybe OpenHiding) (Maybe OpenRenaming)
  -- | @<pat|copat> <name> <names...> = <pattern>@
  | PatSyn Bool Text [Text] Pattern
  -- | @<pat|copat> <names...> [: <type>]?@
  | PatDecl Bool [Text] (Maybe Type)
  -- | @complete <names...> [: <type>]?@
  | Complete [Text] (Maybe Type)
  -- | @infix <fixity> <names...>@
  | FixityDecl Fixity (NonEmpty Text)
  -- | @implicit <expr> [: <type>]?@
  | Implicit Expr (Maybe Type)
  -- | @data <expr>@
  | DataStmt Expr
  -- | @axiom <expr>@
  | AxiomStmt Expr
  -- | @axioms <scope>@
  | AxiomsStmt Scope
  -- | @type <type decl>@
  | TypeStmt TypeDecl
  -- | @types <scope>@
  | TypesStmt Scope
  -- | @trait <trait decl>@
  | TraitStmt TraitDecl
  -- | @record <record impl>@
  | RecordStmt RecordImpl
  deriving Show

-- | @using (pub? pat? <name>; ...)@
type OpenUsing = [(Visibility, Bool, Text)]

-- | @hiding (complete <name>+ or @pat? <name>; ...)@
type OpenHiding = [Either [Text] (Bool, Text)]

-- | @renaming (pat? <name> → pub? <fixity>? <name>; ...)@
type OpenRenaming = [(Text, Text)]

-- | @fixity <associativity>? <precedence>@
data Fixity = Fixity (Maybe Associativity) Precedence

-- | @<left|right>@
data Associativity = AssocLeft | AssocRight

-- | @<+|->?digit+[.digit+]?@
type Precedence = Text

-- | An expression which represents the type of a type.
type Kind = Type
-- | An expression which represents the type of a value.
type Type = Expr
  | Complete [Text] (Maybe Type)
  -- | @infix <fixity> <names...>@
  | FixityDecl Fixity (NonEmpty Text)
  -- | @implicit <expr> [: <type>]?@
  | Implicit Expr (Maybe Type)
  -- | @data <expr>@
  | DataStmt Expr
  -- | @axiom <expr>@
  | AxiomStmt Expr
  -- | @axioms <scope>@
  | AxiomsStmt Scope
  -- | @type <type decl>@
  | TypeStmt TypeDecl
  -- | @types <scope>@
  | TypesStmt Scope
  -- | @trait <trait decl>@
  | TraitStmt TraitDecl
  -- | @record <record impl>@
  | RecordStmt RecordImpl
  deriving Show

-- | @using (pub? pat? <name>; ...)@
type OpenUsing = [(Visibility, Bool, Text)]

-- | @hiding (complete <name>+ or @pat? <name>; ...)@
type OpenHiding = [Either [Text] (Bool, Text)]

-- | @renaming (pat? <name> → pub? <fixity>? <name>; ...)@
type OpenRenaming = [(Text, Text)]

-- | @fixity <associativity>? <precedence>@
data Fixity = Fixity (Maybe Associativity) Precedence

-- | @<left|right>@
data Associativity = AssocLeft | AssocRight

-- | @<+|->?digit+[.digit+]?@
type Precedence = Text

-- | An expression which represents the type of a type.
type Kind = Type
-- | An expression which represents the type of a value.
type Type = Expr

data Expr
  = Data Expr
  | Axiom Expr
  -- | A signature literal. See 'ADT', 'GADT', and 'Sig' for examples.
  | SigE Sig
  | Mod [Either (NonEmpty Text, Kind) Text] (Maybe Type) [TopLevel]
  | Access Expr Text

  ---
  --- Expressions pertaining to types
  ---

  | Forall (NonEmpty (Either (NonEmpty Text, Kind) Text)) Type
  | Arrow [Text] Type Type
  -- | Type annotations (or kind annotations; they are the same thing).
  --
  -- > foo : bar
  | Ann Expr Type
  | Hole

  ---
  --- Expressions pertaining to values
  ---

  | TypeLam (NonEmpty (Either (NonEmpty Text, Kind) Text)) Expr
  | TypeApp Expr Type
  | Lam      [Pattern] [CaseBranch]
  -- | A lambda with only one, trivial branch, e.g. @λ x y z -> e@.
  --
  -- See 'Lam'.
  | LamBlock [Pattern]  Expr
  | App Expr (NonEmpty Expr)
  | AppI Text (NonEmpty Expr)
  | Let [TopLevel] Expr
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

data Sig
  = SigQs [Either (NonEmpty Text, Kind) Text] Sig'
  deriving Show

data Sig'
  = ADT Bool Text (Maybe Kind) [(Text, [Type])]
  | GADT Text (Maybe Kind) [(NonEmpty Text, Type)]
  | Sig [Either (NonEmpty Text, Type) TopLevel]
  deriving Show

data Pattern
  = PatVar Text (Maybe Type)
  -- | @_@
  | Irrelevant
  -- | @!@
  | Impossible
  | PatApp Text [Pattern]
  | PatAppI Text (NonEmpty Pattern)
  -- | @<pat> ← <expr>@
  | View Expr Pattern
  | PatLit (Lit Pattern)
  deriving Show

-- | @<pat> [| <pat> ...]? <refinements...>? [<guard>? → <expr>]?@
data CaseBranch
  = Refinement [Pattern] [Expr] (NonEmpty CaseBranch)

  | Guards     [Pattern]        (NonEmpty (Guard, Expr))
  -- | A case branch without refinement or guards.
  | PlainCase  [Pattern]         Expr
  deriving Show

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
