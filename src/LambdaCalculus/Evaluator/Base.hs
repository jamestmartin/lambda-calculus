module LambdaCalculus.Evaluator.Base
  ( Identity (..)
  , Expr (..), Ctr (..), Pat, ExprF (..), PatF (..), VoidF, UnitF (..), Text
  , substitute, substitute1, rename, rename1, free, bound, used
  , Eval, EvalExpr, EvalExprF, EvalX, EvalXF (..)
  , pattern AppFE, pattern CtrE, pattern CtrFE
  , pattern ContE, pattern ContFE, pattern CallCCE, pattern CallCCFE
  ) where

import LambdaCalculus.Expression.Base

import Control.Monad (forM)
import Control.Monad.Reader (asks)
import Data.Bifunctor (first)
import Data.Foldable (fold)
import Data.Functor.Identity (Identity (..))
import Data.Functor.Foldable (embed, cata, para)
import Data.HashMap.Strict qualified as HM
import Data.Traversable (for)

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
  = ContE_ !r
  -- | Call-with-current-continuation, an evaluator built-in function.
  | CallCCE_
  deriving (Eq, Functor, Foldable, Traversable, Show)

pattern CtrE :: Ctr -> EvalExpr
pattern CtrE c = Ctr c Unit

pattern CtrFE :: Ctr -> EvalExprF r
pattern CtrFE c = CtrF c Unit

pattern ContE :: EvalExpr -> EvalExpr
pattern ContE e = ExprX (ContE_ e)

pattern CallCCE :: EvalExpr
pattern CallCCE = ExprX CallCCE_

pattern ContFE :: r -> EvalExprF r
pattern ContFE e = ExprXF (ContE_ e)

pattern CallCCFE :: EvalExprF r
pattern CallCCFE = ExprXF CallCCE_

pattern AppFE :: r -> r -> EvalExprF r
pattern AppFE ef ex = AppF ef (Identity ex)

{-# COMPLETE Var,  App,   Abs,  Let,  Ctr,   Case,  ContE,  CallCCE  #-}
{-# COMPLETE VarF, AppF,  AbsF, LetF, CtrF,  CaseF, ContFE, CallCCFE #-}
{-# COMPLETE VarF, AppFE, AbsF, LetF, CtrF,  CaseF, ExprXF           #-}
{-# COMPLETE VarF, AppFE, AbsF, LetF, CtrF,  CaseF, ContFE, CallCCFE #-}
{-# COMPLETE Var,  App,   Abs,  Let,  CtrE,  Case,  ContE,  CallCCE  #-}
{-# COMPLETE VarF, AppF,  AbsF, LetF, CtrFE, CaseF, ContFE, CallCCFE #-}
{-# COMPLETE VarF, AppFE, AbsF, LetF, CtrFE, CaseF, ExprXF           #-}
{-# COMPLETE VarF, AppFE, AbsF, LetF, CtrFE, CaseF, ContFE, CallCCFE #-}

instance RecursivePhase Eval where
  projectAppArgs = Identity
  embedAppArgs = runIdentity

instance Substitutable EvalExpr where
  collectVars withVar withBinder = cata \case
    VarF n -> withVar n
    AbsF n e -> withBinder n e
    CaseF pats -> foldMap (\(Pat _ ns e) -> foldr withBinder e ns) pats
    e -> fold e

  rename = runRenamer $ \badNames -> cata \case
    VarF n -> asks $ Var . HM.findWithDefault n n
    AbsF n e -> uncurry Abs . first runIdentity <$> replaceNames badNames (Identity n) e
    ContFE e -> ContE <$> e
    CaseF ps -> Case <$> forM ps \(Pat ctr ns e) -> uncurry (Pat ctr) <$> replaceNames badNames ns e
    e -> embed <$> sequenceA e

  unsafeSubstitute = runSubstituter $ para \case
    VarF name -> asks $ HM.findWithDefault (Var name) name
    AbsF name e -> Abs name <$> maySubstitute (Identity name) e
    ContFE e -> ContE <$> maySubstitute (Identity "!") e
    CaseF pats -> Case <$> for pats \(Pat ctr ns e) -> Pat ctr ns <$> maySubstitute ns e
    e -> embed <$> traverse snd e
