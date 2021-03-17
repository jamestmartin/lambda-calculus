module LambdaCalculus.Syntax.Base
  ( Expr (..), ExprF (..), Def, DefF (..), VoidF, Text, NonEmpty (..)
  , Parse, AST, ASTF, NonEmptyDefFs (..)
  , pattern LetFP
  , simplify
  ) where

import LambdaCalculus.Expression.Base

import Data.Functor.Foldable (embed, project)
import Data.List.NonEmpty (NonEmpty (..))

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
type instance XExpr   Parse = VoidF AST

type ASTF = ExprF Parse
type instance AppArgsF Parse = NonEmpty
type instance LetArgsF Parse = NonEmptyDefFs
type instance XExprF   Parse = VoidF

instance RecursivePhase Parse where
  projectLetArgs = NonEmptyDefFs
  embedLetArgs = getNonEmptyDefFs

newtype NonEmptyDefFs r = NonEmptyDefFs { getNonEmptyDefFs :: NonEmpty (Text, r) }
  deriving (Eq, Functor, Foldable, Traversable, Show)

pattern LetFP :: NonEmpty (Text, r) -> r -> ASTF r
pattern LetFP ds e = LetF (NonEmptyDefFs ds) e

{-# COMPLETE VarF, AppF, AbsF, LetFP, ExprXF #-}

-- | Combine nested expressions into compound expressions when possible.
simplify :: AST -> AST
simplify = simplify' . embed . fmap simplify' . project
  where
    simplify' (App (App f es1) es2) = simplify' $ App f (es1 <> es2)
    simplify' (App (Abs (nx :| ns) eb) (ex :| es)) = simplify' $ app' es $ Let ((nx, ex) :| []) $ abs' ns eb
      where app' []        e = e
            app' (ex2:es2) e = App e (ex2 :| es2)

            abs' []        e = e
            abs' (nx2:ns2) e = Abs (nx2 :| ns2) e
    simplify' (Abs ns1 (Abs ns2 e)) = simplify' $ Abs (ns1 <> ns2) e
    simplify' (Let ds1 (Let ds2 e)) = simplify' $ Let (ds1 <> ds2) e
    simplify' e = e
