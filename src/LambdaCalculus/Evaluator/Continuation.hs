module LambdaCalculus.Evaluator.Continuation
  ( Continuation, continue, continue1
  , ContinuationCrumb (..)
  ) where

import LambdaCalculus.Evaluator.Base

import Data.List (foldl')

data ContinuationCrumb
  -- | The one-hole context of a function application: `(_ e)`
  = ApplyTo EvalExpr
  -- | The one-hole context of the argument to a function application: `(f _)`
  | AppliedTo EvalExpr
  -- | The one-hole context of the body of a lambda abstraction: `(λx. _)`
  | AbstractedOver Text

type Continuation = [ContinuationCrumb]

continue1 :: EvalExpr -> ContinuationCrumb -> EvalExpr
continue1 e (ApplyTo x) = App e x
continue1 e (AppliedTo x) = App x e
continue1 e (AbstractedOver name) = Abs name e

continue :: EvalExpr -> Continuation -> EvalExpr
continue = foldl' continue1
