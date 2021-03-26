module Ivo.Syntax.Base
  ( Expr (..), ExprF (..), Ctr (..), Pat, Def, DefF (..), PatF (..), VoidF, Text, NonEmpty (..)
  , Type (..), TypeF (..), Scheme (..), tapp
  , substitute, substitute1, rename, rename1, free, bound, used
  , Parse, AST, ASTF, ASTX, ASTXF (..), NonEmptyDefFs (..)
  , pattern LetFP, pattern LetRecP, pattern LetRecFP
  , pattern PNat, pattern PNatF, pattern PList, pattern PListF, pattern PChar, pattern PCharF
  , pattern PStr, pattern PStrF, pattern HoleP, pattern HoleFP
  , simplify
  ) where

import Ivo.Expression.Base

import Data.Functor.Foldable (embed, project)
import Data.List.NonEmpty (NonEmpty (..), toList)
import Data.Text qualified as T

data Parse
-- | The abstract syntax tree reflects the structure of the externally-visible syntax.
--
-- It includes syntactic sugar, which allows multiple ways to express many constructions,
-- e.g. multiple definitions in a single let expression or multiple names bound by one abstraction.
type AST = Expr Parse
-- There is no technical reason that the AST can't allow nullary applications and so forth,
-- nor is there any technical reason that the parser couldn't parse them,
-- but the parser *does* reject them to avoid confusing edge cases like `let x=in`,
-- so they're forbidden here too so that the syntax tree can't contain data
--
-- that the parser would refuse to accept.
-- As a matter of curiosity, here's why `let x=in` was syntactically valid:
-- 1. Parentheses in `let` statements are optional, infer them: `let x=()in()`.
-- 2. Insert optional whitespace: `let x = () in ()`.
-- 3. Nullary application expands to the identity function because
--    the identity function is the left identity of function application:
--    `let x=(\x.x) in \x.x`.
type instance AppArgs Parse = NonEmpty AST
type instance AbsArgs Parse = NonEmpty Text
type instance LetArgs Parse = NonEmpty (Def Parse)
type instance CtrArgs Parse = [AST]
type instance AnnX    Parse = ()
type instance XExpr   Parse = ASTX

type ASTX = ASTXF AST

type ASTF = ExprF Parse
type instance AppArgsF Parse = NonEmpty
type instance LetArgsF Parse = NonEmptyDefFs
type instance CtrArgsF Parse = []
type instance XExprF   Parse = ASTXF

data ASTXF r
  -- | A let expression where the definitions may reference each other recursively.
  = LetRecP_ !(Text, r) !r
  -- | A natural number literal, e.g. `10`.
  | PNat_ Int
  -- | A list literal, e.g. `[x, y, z]`.
  | PList_ [r]
  -- | A character literal, e.g. `'a`.
  | PChar_ Char
  -- | A string literal, e.g. `"abcd"`.
  | PStr_ Text
  -- | A type hole.
  | HoleP_
  deriving (Eq, Functor, Foldable, Traversable, Show)

instance RecursivePhase Parse where
  projectLetArgs = NonEmptyDefFs
  embedLetArgs = getNonEmptyDefFs

newtype NonEmptyDefFs r = NonEmptyDefFs { getNonEmptyDefFs :: NonEmpty (Text, r) }
  deriving (Eq, Functor, Foldable, Traversable, Show)

pattern LetFP :: NonEmpty (Text, r) -> r -> ASTF r
pattern LetFP ds e = LetF (NonEmptyDefFs ds) e

pattern LetRecP :: (Text, AST) -> AST -> AST
pattern LetRecP d e = ExprX (LetRecP_ d e)

pattern LetRecFP :: (Text, r) -> r -> ASTF r
pattern LetRecFP d e = ExprXF (LetRecP_ d e)

pattern PNat :: Int -> AST
pattern PNat n = ExprX (PNat_ n)

pattern PNatF :: Int -> ASTF r
pattern PNatF n = ExprXF (PNat_ n)

pattern PList :: [AST] -> AST
pattern PList es = ExprX (PList_ es)

pattern PListF :: [r] -> ASTF r
pattern PListF es = ExprXF (PList_ es)

pattern PChar :: Char -> AST
pattern PChar c = ExprX (PChar_ c)

pattern PCharF :: Char -> ASTF r
pattern PCharF c = ExprXF (PChar_ c)

pattern PStrF :: Text -> ASTF r
pattern PStrF s = ExprXF (PStr_ s)

pattern PStr :: Text -> AST
pattern PStr s = ExprX (PStr_ s)

pattern HoleP :: AST
pattern HoleP = ExprX HoleP_

pattern HoleFP :: ASTF r
pattern HoleFP = ExprXF HoleP_

{-# COMPLETE VarF, AppF, AbsF, LetFP, CtrF, CaseF, AnnF, ExprXF                               #-}
{-# COMPLETE Var,  App,  Abs,  Let,   Ctr,  Case,  Ann,  LetRecP,  PNat,  PList,  PChar,  PStr,  HoleP  #-}
{-# COMPLETE VarF, AppF, AbsF, LetF , CtrF, CaseF, AnnF, LetRecFP, PNatF, PListF, PCharF, PStrF, HoleFP #-}
{-# COMPLETE VarF, AppF, AbsF, LetFP, CtrF, CaseF, AnnF, LetRecFP, PNatF, PListF, PCharF, PStrF, HoleFP #-}

-- TODO: Implement Substitutable for AST.

-- | Combine nested expressions into compound expressions or literals when possible.
simplify :: AST -> AST
simplify = simplify' . embed . fmap simplify . project
  where
    -- Combine sequences of nat constructors into literals.
    simplify' (Ctr CZero [])                  = PNat 0
    simplify' (Ctr CSucc [PNat n])            = PNat (n + 1)
    -- Combine sequences of string constructors into string literals.
    simplify' (Ctr CChar [PNat n])            = PChar (toEnum n)
    simplify' o@(Ctr CCons [PChar c, PList []])
      | c /= '"' = PStr (T.singleton c)
      | otherwise = o
    simplify' o@(Ctr CCons [PChar c, PStr  cs])
      | c /= '"' = PStr (T.cons c cs)
      | otherwise = o
    -- Combine sequences of list contructors into list literals.
    simplify' (Ctr CNil  [])                  = PList []
    simplify' (Ctr CCons [x, PList xs])       = PList (x : xs)
    -- Move applications into constructors.
    simplify' (App (Ctr ctr es1) es2) = simplify' $ Ctr ctr (es1 <> toList es2)
    -- Combine reducible applications into let expressions.
    simplify' (App (Abs (nx :| ns) eb) (ex :| es)) = simplify' $ app' es $ Let ((nx, ex) :| []) $ abs' ns eb
      where app' []        e = e
            app' (ex2:es2) e = App e (ex2 :| es2)

            abs' []        e = e
            abs' (nx2:ns2) e = Abs (nx2 :| ns2) e
    -- Combine sequences of nested applications into n-ary applications.
    simplify' (App (App f es1) es2) = simplify' $ App f (es1 <> es2)
    -- Combine sequences of nested abstractions into n-argument abstractions.
    simplify' (Abs ns1 (Abs ns2 e)) = simplify' $ Abs (ns1 <> ns2) e
    -- Combine sequences of nested let expressions into n-definition let expressions.
    simplify' (Let ds1 (Let ds2 e)) = simplify' $ Let (ds1 <> ds2) e
    simplify' e = e
