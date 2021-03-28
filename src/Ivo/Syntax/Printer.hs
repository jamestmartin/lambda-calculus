-- | Turn abstract syntax into the corresponding concrete syntax.
--
-- This is /not/ a pretty-printer; it uses minimal whitespace)
module Ivo.Syntax.Printer (unparseScope, unparseTopLevel, unparseExpr) where

import Ivo.Syntax.Base

import Data.Functor.Base (NonEmptyF (NonEmptyF))
import Data.Functor.Foldable (cata)
import Data.List.NonEmpty (toList)
import Data.Text qualified as T
import Data.Text.Lazy (fromStrict, toStrict, intercalate, unwords, singleton)
import Data.Text.Lazy.Builder (Builder, fromText, fromLazyText, toLazyText, fromString)
import Prelude hiding (unwords)

unparseScope :: Scope -> Text
unparseScope = unambiguous . scope

unparseTopLevel :: TopLevel -> Text
unparseTopLevel = unambiguous . topLevel

unparseExpr :: Expr -> Text
unparseExpr = unambiguous . expr

type Unparser a = a -> Tagged Builder

scope :: Unparser Scope
scope (Scope tls) = tag Ambiguous $ unmany (topLevel <? ";\n") tls

(<?) :: Unparser a -> Text -> Unparser a
u <? txt = \x -> u x <> fromText txt

topLevel :: Unparser TopLevel
topLevel = \case
  Open pub expr use hide rename ->
    publicOrPrivate pub <>
    unambiguousExpr expr <> " " <>
    openUsing use <> " "
    openHiding hide <> " "
    openRenaming rename
  Define pub def ->
    publicOrPrivate pub <>
    definition def
  TLData expr -> data_ expr
  TLAxiom expr -> axiom expr
  TLSig s -> sig s

publicOrPrivate :: PublicOrPrivate -> Builder
publicOrPrivate Public = "pub "
publicOrPrivate Private = ""

openUsing, openHiding :: [(Text, Maybe (Maybe [Text]))] -> Builder

-- FIXME
openUsing _ = "using (some stuff) "

-- FIXME
openHiding _ = "hiding (some stuff) "

-- FIXME
openRenaming :: [(Text, Text)]
openRenaming _ = "renaming (some stuff) "

definition :: Unparser Definition
definition (Definition name ty expr) =
  fromText name <> " " <> typeSig ty <> "= " <> unambiguousExpr expr

expr :: Unparser Expr
expr = \case
  Data expr -> tag Block $ data_ expr
  Axiom expr -> tag Block $ axiom expr
  SigE s -> tag Block $ sig s
  -- FIXME
  Mod _ _ _ -> tag Block $ error "I don't know how to unparse modules"
  Access expr name -> tag Ambiguous $ blockExpr expr <> "::" <> fromText name
  Forall binders ty -> tag Block $ "∀ " <> typeBinders binders <> "→ " <> ambiguousExpr ty
  Arrow [] arg ty -> tag Ambiguous $ blockExpr arg <> "→ " <> ambiguousExpr ty
  Ann expr ty -> tag Ambiguous $ blockExpr expr <> ": " <> ambiguousExpr ty
  Hole -> tag Finite "_"
  TypeLam binders ty -> tag Block $ "Λ " <> typeBinders binders <> "→ " <> ambiguousExpr ty
  TypeApp expr ty -> tag Ambiguous $ blockExpr <> "@" <> finiteExpr ty
  Lam pats cases -> tag Block $ "λ " <> unmany1 pattern_ pats <> caseBranches cases

data_ = undefined

axiom = undefined

typeBinders :: Unparser (NonEmpty (Either (NonEmpty Text, Kind) Text))
typeBinders = error "Whatever"

sig :: Unparser Sig
sig = error "I don't know how to unparse signatures"

pattern_ :: Unparser Pattern
pattern_ = error "I don't know how to unparse patterns"

caseBranch :: Unparser CaseBranch
caseBranch = error "I don't know how to unparse case branches"

caseBranches :: Unparser [CaseBranch]
caseBranches = error "I don't know how to unparse case branches"

guard :: Unparser Guard
guard = error "I don't know how to unparse guard clauses"

lit :: Unparser a -> Unparser (Lit a)
lit u = error "I don't know how to unparse literals"

-- I'm surprised this isn't in base somewhere.
unsnoc :: NonEmpty a -> ([a], a)
unsnoc = cata \case
  NonEmptyF x' Nothing -> ([], x')
  NonEmptyF x (Just (xs, x')) -> (x : xs, x')

data SyntaxType
  -- | Ambiguous syntax is not necessarily finite and not guaranteed to consume any input.
  = Ambiguous
  -- | Block syntax is not necessarily finite but is guaranteed to consume input.
  | Block
  -- | Unambiguous syntax is finite and guaranteed to consume input.
  | Finite
type Tagged a = (SyntaxType, a)

tag :: SyntaxType -> a -> Tagged a
tag = (,)

group :: Builder -> Builder
group x = "(" <> x <> ")"

-- | An unambiguous context has a marked beginning and end.
unambiguous :: Tagged Builder -> Builder
unambiguous (_, t) = t

-- | A final context has a marked end but no marked beginning,
-- so we provide a grouper when a beginning marker is necessary.
final :: Tagged Builder -> Builder
final (Ambiguous, t) = group t
final (_, t) = t

-- | An ambiguous context has neither a marked end nor marked beginning,
-- so we provide a grouper when an ending marker is necessary.
ambiguous :: Tagged Builder -> Builder
ambiguous (Finite, t) = t
ambiguous (_, t) = group t

ambiguousExpr, blockExpr, finiteExpr :: Expr -> Builder

ambiguousExpr = ambiguous . expr

blockExpr = final . expr

finiteExpr = unambiguous . expr

unmany :: Foldable t => Unparser a -> t a -> Builder
unmany u xs = error "fuck it, implement this later"
