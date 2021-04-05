module Ivo.Types.Base
  ( Identity (..)
  , Expr (..), Ctr (..), Pat, ExprF (..), PatF (..), VoidF, UnitF (..), Text
  , Type (..), TypeF (..), Scheme (..), tapp
  , substitute, substitute1, rename, rename1, free, bound, used
  , Check, CheckExpr, CheckExprF, CheckLet, CheckLetF (..), CheckX, CheckXF (..)
  , pattern AppFC, pattern LetC, pattern LetFC
  , pattern CtrC, pattern CtrFC, pattern CallCCC, pattern CallCCFC
  , pattern FixC, pattern FixFC, pattern HoleC, pattern HoleFC
  , Substitution, Context, Constraint
  , MonoSubstitutable, substituteMono, substituteMono1
  ) where

import Ivo.Expression.Base

import Control.Monad (forM)
import Control.Monad.Reader (asks)
import Data.Bifunctor (bimap, first)
import Data.Foldable (fold)
import Data.Functor.Foldable (embed, cata, para)
import Data.Functor.Identity (Identity (..))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Traversable (for)

data Check
type CheckExpr = Expr Check
type instance AppArgs Check = CheckExpr
type instance AbsArgs Check = Text
type instance LetArgs Check = CheckLet
type instance CtrArgs Check = UnitF CheckExpr
type instance AnnX    Check = ()
type instance XExpr   Check = CheckX

type CheckLet = CheckLetF CheckExpr
type CheckX = CheckXF CheckExpr

type CheckExprF = ExprF Check
type instance AppArgsF Check = Identity
type instance LetArgsF Check = CheckLetF
type instance CtrArgsF Check = UnitF
type instance XExprF   Check = CheckXF

data CheckLetF r = CheckLet Text (Maybe Type) r
  deriving (Eq, Functor, Foldable, Traversable, Show)

data CheckXF r
  -- | Call-with-current-continuation.
  = CallCCC_
  -- | A fixpoint combinator,
  -- because untyped lambda calculus fixpoint combinators won't typecheck.
  | FixC_
  -- | A hole to ask the type inferencer about the context for debugging purposes.
  | HoleC_
  deriving (Eq, Functor, Foldable, Traversable, Show)

pattern LetC :: Text -> Maybe Type -> CheckExpr -> CheckExpr -> CheckExpr
pattern LetC name ty expr body = Let (CheckLet name ty expr) body

pattern LetFC :: Text -> Maybe Type -> r -> r -> CheckExprF r
pattern LetFC name ty expr body = LetF (CheckLet name ty expr) body

pattern CtrC :: Ctr -> CheckExpr
pattern CtrC c = Ctr c Unit

pattern CtrFC :: Ctr -> CheckExprF r
pattern CtrFC c = CtrF c Unit

pattern AppFC :: r -> r -> CheckExprF r
pattern AppFC ef ex = AppF ef (Identity ex)

pattern CallCCC :: CheckExpr
pattern CallCCC = ExprX CallCCC_

pattern CallCCFC :: CheckExprF r
pattern CallCCFC = ExprXF CallCCC_

pattern FixC :: CheckExpr
pattern FixC = ExprX FixC_

pattern FixFC :: CheckExprF r
pattern FixFC = ExprXF FixC_

pattern HoleC :: CheckExpr
pattern HoleC = ExprX HoleC_

pattern HoleFC :: CheckExprF r
pattern HoleFC = ExprXF HoleC_

{-# COMPLETE Var,  App,   Abs,  Let,   CtrC,  Case,  Ann,  ExprX                   #-}
{-# COMPLETE VarF, AppF,  AbsF, LetF,  CtrFC, CaseF, AnnF, ExprXF                  #-}
{-# COMPLETE VarF, AppFC, AbsF, LetF,  CtrF,  CaseF, AnnF, ExprXF                  #-}
{-# COMPLETE VarF, AppFC, AbsF, LetF,  CtrFC, CaseF, AnnF, ExprXF                  #-}
{-# COMPLETE Var,  App,   Abs,  Let,   Ctr,   Case,  Ann,  CallCCC,  FixC,  HoleC  #-}
{-# COMPLETE Var,  App,   Abs,  Let,   CtrC,  Case,  Ann,  CallCCC,  FixC,  HoleC  #-}
{-# COMPLETE VarF, AppF,  AbsF, LetF,  CtrFC, CaseF, AnnF, CallCCFC, FixFC, HoleFC #-}
{-# COMPLETE VarF, AppFC, AbsF, LetF,  CtrF,  CaseF, AnnF, CallCCFC, FixFC, HoleFC #-}
{-# COMPLETE VarF, AppFC, AbsF, LetF,  CtrFC, CaseF, AnnF, CallCCFC, FixFC, HoleFC #-}
{-# COMPLETE Var,  App,   Abs,  LetC,  CtrC,  Case,  Ann,  ExprX                   #-}
{-# COMPLETE VarF, AppF,  AbsF, LetFC, CtrFC, CaseF, AnnF, ExprXF                  #-}
{-# COMPLETE VarF, AppFC, AbsF, LetFC, CtrF,  CaseF, AnnF, ExprXF                  #-}
{-# COMPLETE VarF, AppFC, AbsF, LetFC, CtrFC, CaseF, AnnF, ExprXF                  #-}
{-# COMPLETE Var,  App,   Abs,  LetC,  Ctr,   Case,  Ann,  CallCCC,  FixC,  HoleC  #-}
{-# COMPLETE Var,  App,   Abs,  LetC,  CtrC,  Case,  Ann,  CallCCC,  FixC,  HoleC  #-}
{-# COMPLETE VarF, AppF,  AbsF, LetFC, CtrFC, CaseF, AnnF, CallCCFC, FixFC, HoleFC #-}
{-# COMPLETE VarF, AppFC, AbsF, LetFC, CtrF,  CaseF, AnnF, CallCCFC, FixFC, HoleFC #-}
{-# COMPLETE VarF, AppFC, AbsF, LetFC, CtrFC, CaseF, AnnF, CallCCFC, FixFC, HoleFC #-}

instance RecursivePhase Check where
  projectAppArgs = Identity
  embedAppArgs = runIdentity

instance Substitutable CheckExpr where
  collectVars withVar withBinder = cata \case
    VarF n -> withVar n
    AbsF n e -> withBinder n e
    LetF (CheckLet n _ x) e -> x <> withBinder n e
    CaseF pats -> foldMap (\(Pat _ ns e) -> foldr withBinder e ns) pats
    e -> fold e

  rename = runRenamer $ \badNames -> cata \case
    VarF n -> asks $ Var . HM.findWithDefault n n
    AbsF n e -> uncurry Abs . first runIdentity <$> replaceNames badNames (Identity n) e
    LetFC n ty x e -> do
      x' <- x
      (Identity n', e') <- replaceNames badNames (Identity n) e
      pure $ LetC n' ty x' e'
    CaseF ps -> Case <$> forM ps \(Pat ctr ns e) ->
      uncurry (Pat ctr) <$> replaceNames badNames ns e
    e -> embed <$> sequenceA e

  unsafeSubstitute = runSubstituter $ para \case
    VarF name -> asks $ HM.findWithDefault (Var name) name
    AbsF name e -> Abs name <$> maySubstitute (Identity name) e
    LetFC name ty (_, x) e -> do
      x' <- x
      e' <- maySubstitute (Identity name) e
      pure $ LetC name ty x' e'
    CaseF pats -> Case <$> for pats \(Pat ctr ns e) -> Pat ctr ns <$> maySubstitute ns e
    e -> embed <$> traverse snd e

type Substitution = HashMap Text Type
type Context = HashMap Text Scheme
type Constraint = (Type, Type)

class MonoSubstitutable t where
  substituteMono :: Substitution -> t -> t

  substituteMono1 :: Text -> Type -> t -> t
  substituteMono1 var val= substituteMono (HM.singleton var val)

instance MonoSubstitutable Type where
  substituteMono = substitute

instance MonoSubstitutable Scheme where
  substituteMono substs (TForall names t) =
    TForall names $ substitute (foldMap HM.delete names substs) t

instance MonoSubstitutable Constraint where
  substituteMono substs = bimap (substituteMono substs) (substituteMono substs)

instance MonoSubstitutable t => MonoSubstitutable [t] where
  substituteMono = fmap . substituteMono

instance MonoSubstitutable t => MonoSubstitutable (HashMap a t) where
  substituteMono = fmap . substituteMono
