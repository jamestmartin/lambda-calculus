module LambdaCalculus.Evaluator.Base
  ( Identity (..)
  , Expr (..), Ctr (..), Pat, ExprF (..), PatF (..), VoidF, UnitF (..), Text
  , Eval, EvalExpr, EvalExprF, EvalX, EvalXF (..)
  , pattern AppFE, pattern CtrE, pattern CtrFE
  , pattern Cont, pattern ContF, pattern CallCC, pattern CallCCF
  ) where

import LambdaCalculus.Expression.Base

import Data.Functor.Identity (Identity (..))

data Eval
type EvalExpr = Expr Eval
type instance AppArgs Eval = EvalExpr
type instance AbsArgs Eval = Text
type instance LetArgs Eval = VoidF EvalExpr
type instance CtrArgs Eval = UnitF EvalExpr
type instance XExpr   Eval = EvalX

type EvalX = EvalXF EvalExpr

type EvalExprF = ExprF Eval
type instance AppArgsF Eval = Identity
type instance LetArgsF Eval = VoidF
type instance CtrArgsF Eval = UnitF
type instance XExprF   Eval = EvalXF

data EvalXF r
   -- | A continuation. This is identical to a lambda abstraction,
   -- with the exception that it performs the side-effect of
   -- deleting the current continuation.
   --
   -- Continuations do not have any corresponding surface-level syntax,
   -- but may be printed like a lambda with the illegal variable `!`.
  = Cont_ !r
  -- | Call-with-current-continuation, an evaluator built-in function.
  | CallCC_
  deriving (Eq, Functor, Foldable, Traversable, Show)

instance RecursivePhase Eval where
  projectAppArgs = Identity
  embedAppArgs = runIdentity

pattern CtrE :: Ctr -> EvalExpr
pattern CtrE c = Ctr c Unit

pattern CtrFE :: Ctr -> EvalExprF r
pattern CtrFE c = CtrF c Unit

pattern Cont :: EvalExpr -> EvalExpr
pattern Cont e = ExprX (Cont_ e)

pattern CallCC :: EvalExpr
pattern CallCC = ExprX CallCC_

pattern ContF :: r -> EvalExprF r
pattern ContF e = ExprXF (Cont_ e)

pattern CallCCF :: EvalExprF r
pattern CallCCF = ExprXF CallCC_

pattern AppFE :: r -> r -> EvalExprF r
pattern AppFE ef ex = AppF ef (Identity ex)

{-# COMPLETE Var,  App,   Abs,  Let,  Ctr,   Case,  Cont,  CallCC  #-}
{-# COMPLETE VarF, AppF,  AbsF, LetF, CtrF,  CaseF, ContF, CallCCF #-}
{-# COMPLETE VarF, AppFE, AbsF, LetF, CtrF,  CaseF, ExprXF         #-}
{-# COMPLETE VarF, AppFE, AbsF, LetF, CtrF,  CaseF, ContF, CallCCF #-}
{-# COMPLETE Var,  App,   Abs,  Let,  CtrE,  Case,  Cont,  CallCC  #-}
{-# COMPLETE VarF, AppF,  AbsF, LetF, CtrFE, CaseF, ContF, CallCCF #-}
{-# COMPLETE VarF, AppFE, AbsF, LetF, CtrFE, CaseF, ExprXF         #-}
{-# COMPLETE VarF, AppFE, AbsF, LetF, CtrFE, CaseF, ContF, CallCCF #-}
