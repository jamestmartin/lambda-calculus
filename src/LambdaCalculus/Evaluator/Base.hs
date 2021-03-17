module LambdaCalculus.Evaluator.Base
  ( Identity (..)
  , Expr (..), ExprF (..), VoidF, Text
  , Eval, EvalExpr, EvalExprF, EvalX, EvalXF (..)
  , pattern AppFE, pattern Cont, pattern ContF
  ) where

import LambdaCalculus.Expression.Base

import Data.Functor.Identity (Identity (..))

data Eval
type EvalExpr = Expr Eval
type instance AppArgs Eval = EvalExpr
type instance AbsArgs Eval = Text
type instance LetArgs Eval = VoidF EvalExpr
type instance XExpr   Eval = EvalX

type EvalX = EvalXF EvalExpr

type EvalExprF = ExprF Eval
type instance AppArgsF Eval = Identity
type instance LetArgsF Eval = VoidF
type instance XExprF   Eval = EvalXF

newtype EvalXF r
   -- | A continuation. This is identical to a lambda abstraction,
   -- with the exception that it performs the side-effect of
   -- deleting the current continuation.
   --
   -- Continuations do not have any corresponding surface-level syntax,
   -- but may be printed like a lambda with the illegal variable `!`.
  = Cont_ r
  deriving (Eq, Functor, Foldable, Traversable, Show)

instance RecursivePhase Eval where
  projectAppArgs = Identity
  embedAppArgs = runIdentity

pattern Cont :: EvalExpr -> EvalExpr
pattern Cont e = ExprX (Cont_ e)

pattern ContF :: r -> EvalExprF r
pattern ContF e = ExprXF (Cont_ e)

pattern AppFE :: r -> r -> EvalExprF r
pattern AppFE ef ex = AppF ef (Identity ex)

{-# COMPLETE Var,  App,   Abs,  Let,  Cont   #-}
{-# COMPLETE VarF, AppF,  AbsF, LetF, ContF  #-}
{-# COMPLETE VarF, AppFE, AbsF, LetF, ExprXF #-}
{-# COMPLETE VarF, AppFE, AbsF, LetF, ContF  #-}
