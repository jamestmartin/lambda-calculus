module LambdaCalculus.Syntax.Printer (unparseAST) where

import LambdaCalculus.Syntax.Base

import Data.Functor.Base (NonEmptyF (NonEmptyF))
import Data.Functor.Foldable (cata)
import Data.List.NonEmpty (toList)
import Data.Text.Lazy (fromStrict, toStrict, intercalate, unwords)
import Data.Text.Lazy.Builder (Builder, fromText, fromLazyText, toLazyText)
import Prelude hiding (unwords)

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

-- | Turn an abstract syntax tree into the corresponding concrete syntax.
--
-- This is *not* a pretty-printer; it uses minimal whitespace.
unparseAST :: AST -> Text
unparseAST = toStrict . toLazyText . snd . cata \case
  VarF name -> tag Finite $ fromText name
  AppF ef (unsnoc -> (exs, efinal)) -> tag Ambiguous $ foldr (\e es' -> ambiguous e <> " " <> es') (final efinal) (ef : exs)
  AbsF names body -> tag Block $
    let names' = fromLazyText (unwords $ map fromStrict $ toList names)
    in "λ" <> names' <> ". " <> unambiguous body
  LetFP defs body -> tag Block $
    let
      unparseDef (name, val) = fromText name <> " = " <> unambiguous val
      defs' = fromLazyText (intercalate "; " $ map (toLazyText . unparseDef) $ toList defs)
    in "let " <> defs' <> " in " <> unambiguous body
