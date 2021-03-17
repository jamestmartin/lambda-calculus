module LambdaCalculus.Expression
  ( Expr (..), ExprF (..), DefF (..), VoidF, Text
  , Eval, EvalExpr, EvalX, EvalXF (..), Identity (..)
  , pattern AppFE, pattern Cont, pattern ContF
  , Parse, AST, ASTF, NonEmptyDefFs (..), NonEmpty (..), simplify
  , pattern LetFP
  , ast2eval, eval2ast
  ) where

import LambdaCalculus.Evaluator.Base
import LambdaCalculus.Syntax.Base

import Data.Functor.Foldable (cata, hoist)
import Data.List (foldl')
import Data.List.NonEmpty (toList)

-- | Convert from an abstract syntax tree to an evaluator expression.
ast2eval :: AST -> EvalExpr
ast2eval = cata \case
  VarF name -> Var name
  AppF ef exs -> foldl' App ef $ toList exs
  AbsF ns e -> foldr Abs e $ toList ns
  LetF ds e ->
    let letExpr name val body' = App (Abs name body') val
    in foldr (uncurry letExpr) e $ getNonEmptyDefFs ds

-- | Convert from an evaluator expression to an abstract syntax tree.
eval2ast :: EvalExpr -> AST
eval2ast = hoist \case
  VarF name -> VarF name
  AppFE ef ex -> AppF ef (ex :| [])
  AbsF n e -> AbsF (n :| []) e
  ContF e -> AbsF ("!" :| []) e
