module LambdaCalculus.Parser.AbstractSyntax
  ( AbstractSyntax (..), AbstractSyntaxF (..), Definition, Identifier
  ) where

import Data.Functor.Base (NonEmptyF (NonEmptyF))
import Data.Functor.Foldable (cata)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.List.NonEmpty (NonEmpty, toList)
import Data.Text (Text)
import Data.Text qualified as T
import TextShow (Builder, TextShow, showb, showt, toText, fromText)

-- | The abstract syntax tree reflects the structure of the externally-visible syntax.
--
-- This contains a lot of syntactic sugar when compared with 'LambdaCalculus.Expression'.
-- If this syntactic sugar were used in Expression, then operations like evaluation
-- would become unnecessarily complicated, because the same expression
-- can be represented in terms of multiple abstract syntax trees.
data AbstractSyntax
  = Variable Identifier
  -- There is no technical reason for the AST to forbid nullary applications and so forth.
  -- However the parser rejects them to avoid confusing edge cases like `let x=in`,
  -- so they're forbidden here too so that the syntax tree can't contain data
  -- that the parser would refuse to accept.
  --
  -- As a matter of curiosity, here's why `let x=in` was syntactically valid:
  -- 1. Parentheses in `let` statements are optional, infer them: `let x=()in()`.
  -- 2. Insert optional whitespace: `let x = () in ()`.
  -- 3. Nullary application expands to the identity function because
  --    the identity function is the left identity of function application:
  --    `let x=(\x.x) in \x.x`.
  --
  -- | n-ary function application: `(f x_1 x_2 ... x_n)`.
  | Application (NonEmpty AbstractSyntax)
  -- | Lambda abstraction over n variables: `(λx_1 x_2 ... x_n. e)`
  | Abstraction (NonEmpty Identifier) AbstractSyntax
  -- | Let expressions (syntactic sugar) binding `n` variables:
  --   `let x_1 = e_1; x_2 = e_2; ... x_n = e_n`.
  | Let (NonEmpty Definition) AbstractSyntax
type Definition = (Identifier, AbstractSyntax)
type Identifier = Text

makeBaseFunctor ''AbstractSyntax

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

instance TextShow AbstractSyntax where
  showb = snd . cata \case
    VariableF name -> tag Finite $ fromText name
    ApplicationF (unsnoc -> (es, efinal)) -> tag Ambiguous $ foldr (\e es' -> ambiguous e <> " " <> es') (final efinal) es
    AbstractionF names body -> tag Block $
      let names' = fromText (T.intercalate " " $ toList names)
      in "λ" <> names' <> ". " <> unambiguous body
    LetF defs body -> tag Block $
      let
        showDef (name, val) = fromText name <> " = " <> unambiguous val
        defs' = fromText (T.intercalate "; " $ map (toText . showDef) $ toList defs)
      in "let " <> defs' <> " in " <> unambiguous body

instance Show AbstractSyntax where
  show = T.unpack . showt
