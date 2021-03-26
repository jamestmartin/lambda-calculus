module Ivo.Syntax.Printer (unparseAST) where

import Ivo.Syntax.Base

import Data.Functor.Base (NonEmptyF (NonEmptyF))
import Data.Functor.Foldable (cata)
import Data.List.NonEmpty (toList)
import Data.Text.Lazy (fromStrict, toStrict, intercalate, unwords, singleton)
import Data.Text.Lazy.Builder (Builder, fromText, fromLazyText, toLazyText, fromString)
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
  AppF ef exs -> unparseApp ef exs
  AbsF names body -> tag Block $
    let names' = fromLazyText (unwords $ map fromStrict $ toList names)
    in "λ" <> names' <> ". " <> unambiguous body
  LetFP defs body -> tag Block $ "let " <> unparseDefs defs <> " in " <> unambiguous body
  LetRecFP def body -> tag Block $ "letrec " <> unparseDef def <> " in " <> unambiguous body
  CtrF ctr e -> unparseCtr ctr e
  CaseF pats ->
    let pats' = fromLazyText $ intercalate "; " $ map (toLazyText . unparsePat) pats
    in tag Finite $ "{ " <> pats' <> " }"
  PNatF n -> tag Finite $ fromString $ show n
  PListF es ->
    let es' = fromLazyText $ intercalate ", " $ map (toLazyText . unambiguous) es
    in tag Finite $ "[" <> es' <> "]"
  PStrF s -> tag Finite $ "\"" <> fromText s <> "\""
  PCharF c -> tag Finite $ "'" <> fromLazyText (singleton c)
  HoleFP -> tag Finite "_"
  where
    unparseApp :: Tagged Builder -> NonEmpty (Tagged Builder) -> Tagged Builder
    unparseApp ef (unsnoc -> (exs, efinal))
      = tag Ambiguous $ foldr (\e es' -> ambiguous e <> " " <> es') (final efinal) (ef : exs)

    unparseDef (name, val) = fromText name <> " = " <> unambiguous val
    unparseDefs defs = fromLazyText (intercalate "; " $ map (toLazyText . unparseDef) $ toList defs)

    unparseCtr :: Ctr -> [Tagged Builder] -> Tagged Builder
    -- Fully-applied special syntax forms
    unparseCtr CPair [x, y] = tag Finite $ "(" <> unambiguous x <>  ", "  <> unambiguous y <> ")"
    unparseCtr CCons [x, y] = tag Finite $ "(" <> unambiguous x <> " :: " <> unambiguous y <> ")"
    -- Partially-applied syntax forms
    unparseCtr CUnit  [] = tag Finite "()"
    unparseCtr CPair  [] = tag Finite "(,)"
    unparseCtr CLeft  [] = tag Finite "Left"
    unparseCtr CRight [] = tag Finite "Right"
    unparseCtr CZero  [] = tag Finite "Z"
    unparseCtr CSucc  [] = tag Finite "S"
    unparseCtr CNil   [] = tag Finite "[]"
    unparseCtr CCons  [] = tag Finite "(::)"
    unparseCtr CChar  [] = tag Finite "Char"
    unparseCtr ctr (x:xs) = unparseApp (unparseCtr ctr []) (x :| xs)

    unparsePat (Pat ctr ns e)
      = unambiguous (unparseCtr ctr (map (tag Finite . fromText) ns)) <> " -> " <> unambiguous e
