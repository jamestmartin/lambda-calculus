module Ivo.Syntax.Printer
  ( unparseScope, unparseDef, unparseExpr
  ) where

import Ivo.Syntax.Base

import Data.Foldable (foldl', toList)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (Builder, fromText, toLazyText, fromString)
import Prelude hiding (unwords)

unparseScope :: Scope Text -> Text
unparseScope = unparse scope

unparseDef :: Def Text -> Text
unparseDef = unparse def

unparseExpr :: Expr Text -> Text
unparseExpr = unparse expr

scope :: Unparser (Scope Text)
scope (Scope defs) = Block $ flexSepEndBy "; " $ map (ambiguous . def) defs

def :: Unparser (Def Text)
def (BasicDef name body) =
  Block $ fromText name <> " = " <> ambiguous (expr body)
def (BasicDecl names ty) =
  Block $ unnames names <> " : " <> ambiguous (expr ty)

expr :: Unparser (Expr Text)
expr (Var name) = Finite $ fromText name
expr (Lit lit) = Block $ literal expr lit
expr (App ef exs) = Ambiguous $ sepBy " " $ map (finite . expr) $ ef : toList exs
expr (Let defs body) = Block $
  "let " <> ambiguous (scope $ Scope $ toList defs) <>
  " in " <> ambiguous (expr body)
expr (Lam names body) = Block $
  "λ " <> unnames names <> " → " <> ambiguous (expr body)
expr (Case arg branches) = Block $
  "case " <> ambiguous (expr arg) <> " " <> caseBranches branches
expr (Forall names ty) = Block $
  "∀ " <> unnames names <> " ⇒ " <> ambiguous (expr ty)
expr (Arrow arg ty) = Ambiguous $
  block (expr arg) <> " → " <> ambiguous (expr ty)
expr (Ann e ty) = Ambiguous $
  block (expr e) <> " : " <> ambiguous (expr ty)
expr Hole = Finite "_"

caseBranches :: CaseBranches Text -> Builder
caseBranches (CaseBranches pats) = flexBraces $ map caseBranch pats

caseBranch :: (Pattern Text, Expr Text) -> Builder
caseBranch (pat, body) = pattern_ pat <> " → " <> ambiguous (expr body)

pattern_ :: Pattern Text -> Builder
pattern_ (PatVar name) = fromText name
pattern_ (PatLit lit) = literal (Finite . pattern_) lit
pattern_ Irrelevant = "_"
pattern_ (PatApp ctr args) =
  fromText ctr <> " " <> sepBy " " (map pattern_ args)

literal :: Unparser a -> Literal a -> Builder
literal up = \case
  LitInt n
    | n > 0     -> fromString $ "⁻" <> show (abs n)
    | otherwise -> fromString $ show n
  LitChar c -> "‘" <> fromText (T.singleton c) <> "’"
  LitStr s -> "“" <> fromText s <> "”"
  LitList xs -> flexBrackets $ map (ambiguous . up) xs

unnames :: Foldable t => t Text -> Builder
unnames = fromText . T.unwords . toList

between :: Builder -> Builder -> Builder -> Builder
between l r x = l <> x <> r

sepBy, flexSepEndBy :: Foldable t => Builder -> t Builder -> Builder

sepBy delim = foldl' (\x xs -> x <> delim <> xs) ""

flexSepEndBy delim = foldMap (<> delim)

flexBrackets, flexBraces :: Foldable t => t Builder -> Builder

flexBrackets = between "[ " " ]" . sepBy "; "

flexBraces = between "{ " " }" . sepBy "; "

type Unparser a = a -> Tagged Builder

data Tagged a
  = Ambiguous !a
  | Block !a
  | Finite !a

untag :: Tagged a -> a
untag = \case
  Ambiguous x -> x
  Block x -> x
  Finite x -> x

ambiguous :: Tagged Builder -> Builder
ambiguous = untag

block :: Tagged Builder -> Builder
block (Ambiguous x) = parens x
block x = untag x

finite :: Tagged Builder -> Builder
finite (Finite x) = x
finite x = parens $ untag x

parens :: Builder -> Builder
parens = between "(" ")"

unparse :: Unparser a -> a -> Text
unparse up = toStrict . toLazyText . untag . up
