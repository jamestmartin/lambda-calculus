module LambdaCalculus.Expression
  ( Expr (..), ExprF (..), DefF (..), VoidF, Text
  , Eval, EvalExpr, EvalX, EvalXF (..), Identity (..)
  , pattern AppFE, pattern Cont, pattern ContF, pattern CallCC, pattern CallCCF
  , Parse, AST, ASTF, NonEmptyDefFs (..), NonEmpty (..), simplify
  , pattern LetFP
  , ast2eval, eval2ast
  ) where

import LambdaCalculus.Evaluator.Base
import LambdaCalculus.Evaluator
import LambdaCalculus.Syntax.Base

import Data.Functor.Foldable (cata, hoist)
import Data.HashSet qualified as HS
import Data.List (foldl')
import Data.List.NonEmpty (toList)

-- | Convert from an abstract syntax tree to an evaluator expression.
ast2eval :: AST -> EvalExpr
ast2eval = substitute "callcc" CallCC . cata \case
  VarF name -> Var name
  AppF ef exs -> foldl' App ef $ toList exs
  AbsF ns e -> foldr Abs e $ toList ns
  LetF ds e ->
    let letExpr name val body' = App (Abs name body') val
    in foldr (uncurry letExpr) e $ getNonEmptyDefFs ds

-- | Convert from an evaluator expression to an abstract syntax tree.
eval2ast :: EvalExpr -> AST
-- Because all `ast2eval` replaces all free instances of `callcc`,
-- all instances of `callcc` must be bound;
-- therefore, we are free to alpha convert them,
-- freeing the name `callcc` for us to use for the built-in again.
eval2ast = hoist go . alphaConvert (HS.singleton "callcc")
  where
    go :: EvalExprF r -> ASTF r
    go = \case
      VarF name -> VarF name
      CallCCF -> VarF "callcc"
      AppFE ef ex -> AppF ef (ex :| [])
      AbsF n e -> AbsF (n :| []) e
      ContF e -> AbsF ("!" :| []) e
