{-# LANGUAGE TemplateHaskell #-}
module Ivo.Types.Base
  ( Identity (..)
  , Expr (..), Ctr (..), Pat, ExprF (..), PatF (..), VoidF, UnitF (..), Text
  , substitute, substitute1, rename, rename1, free, bound, used
  , Check, CheckExpr, CheckExprF, CheckX, CheckXF (..)
  , pattern AppFC, pattern CtrC, pattern CtrFC, pattern CallCCC, pattern CallCCFC
  , pattern FixC, pattern FixFC, pattern HoleC, pattern HoleFC
  , Type (..), TypeF (..), Scheme (..), tapp
  , Substitution, Context, Constraint
  , MonoSubstitutable, substituteMono, substituteMono1
  , unparseType, unparseScheme
  ) where

import Control.Monad (forM)
import Control.Monad.Reader (asks)
import Data.Bifunctor (bimap, first)
import Data.Foldable (fold)
import Data.Functor.Foldable (embed, cata, para)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Functor.Identity (Identity (..))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List (foldl1')
import Data.Text qualified as T
import Data.Traversable (for)
import Ivo.Expression.Base

data Check
type CheckExpr = Expr Check
type instance AppArgs Check = CheckExpr
type instance AbsArgs Check = Text
type instance LetArgs Check = (Text, CheckExpr)
type instance CtrArgs Check = UnitF CheckExpr
type instance XExpr   Check = CheckX

type CheckX = CheckXF CheckExpr

type CheckExprF = ExprF Check
type instance AppArgsF Check = Identity
type instance LetArgsF Check = DefF
type instance CtrArgsF Check = UnitF
type instance XExprF   Check = CheckXF

data CheckXF r
  -- | Call-with-current-continuation.
  = CallCCC_
  -- | A fixpoint combinator,
  -- because untyped lambda calculus fixpoint combinators won't typecheck.
  | FixC_
  -- | A hole to ask the type inferencer about the context for debugging purposes.
  | HoleC_
  deriving (Eq, Functor, Foldable, Traversable, Show)

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

{-# COMPLETE Var,  App,   Abs,  Let,  CtrC,  Case,  ExprX                   #-}
{-# COMPLETE VarF, AppF,  AbsF, LetF, CtrFC, CaseF, ExprXF                  #-}
{-# COMPLETE VarF, AppFC, AbsF, LetF, CtrF,  CaseF, ExprXF                  #-}
{-# COMPLETE VarF, AppFC, AbsF, LetF, CtrFC, CaseF, ExprXF                  #-}
{-# COMPLETE Var,  App,   Abs,  Let,  Ctr,   Case,  CallCCC,  FixC,  HoleC  #-}
{-# COMPLETE Var,  App,   Abs,  Let,  CtrC,  Case,  CallCCC,  FixC,  HoleC  #-}
{-# COMPLETE VarF, AppF,  AbsF, LetF, CtrFC, CaseF, CallCCFC, FixFC, HoleFC #-}
{-# COMPLETE VarF, AppFC, AbsF, LetF, CtrF,  CaseF, CallCCFC, FixFC, HoleFC #-}
{-# COMPLETE VarF, AppFC, AbsF, LetF, CtrFC, CaseF, CallCCFC, FixFC, HoleFC #-}

instance RecursivePhase Check where
  projectAppArgs = Identity
  projectLetArgs = projectDef

  embedAppArgs = runIdentity
  embedLetArgs = embedDef

instance Substitutable CheckExpr where
  collectVars withVar withBinder = cata \case
    VarF n -> withVar n
    AbsF n e -> withBinder n e
    LetF (Def n x) e -> x <> withBinder n e
    CaseF pats -> foldMap (\(Pat _ ns e) -> foldr withBinder e ns) pats
    e -> fold e

  rename = runRenamer $ \badNames -> cata \case
    VarF n -> asks $ Var . HM.findWithDefault n n
    AbsF n e -> uncurry Abs . first runIdentity <$> replaceNames badNames (Identity n) e
    LetF (Def n x) e -> do
      x' <- x
      (Identity n', e') <- replaceNames badNames (Identity n) e
      pure $ Let (n', x') e'
    CaseF ps -> Case <$> forM ps \(Pat ctr ns e) ->
      uncurry (Pat ctr) <$> replaceNames badNames ns e
    e -> embed <$> sequenceA e

  unsafeSubstitute = runSubstituter $ para \case
    VarF name -> asks $ HM.findWithDefault (Var name) name
    AbsF name e -> Abs name <$> maySubstitute (Identity name) e
    LetF (Def name (_, x)) e -> do
      x' <- x
      e' <- maySubstitute (Identity name) e
      pure $ Let (name, x') e'
    CaseF pats -> Case <$> for pats \(Pat ctr ns e) -> Pat ctr ns <$> maySubstitute ns e
    e -> embed <$> traverse snd e

-- | A monomorphic type.
data Type
  -- | Type variable.
  = TVar Text
  -- | Type application.
  | TApp Type Type
  -- | The function type.
  | TAbs
  -- | The product type.
  | TProd
  -- | The sum type.
  | TSum
  -- | The unit type.
  | TUnit
  -- | The empty type.
  | TVoid
  -- | The type of natural numbers.
  | TNat
  -- | The type of lists.
  | TList
  -- | The type of characters.
  | TChar
  deriving (Eq, Show)

makeBaseFunctor ''Type

instance Substitutable Type where
  collectVars withVar _ = cata \case
    TVarF n -> withVar n
    t -> fold t

  -- /All/ variables in a monomorphic type are free.
  rename _ t = t

  -- No renaming step is necessary.
  substitute substs = cata \case
    TVarF n -> HM.findWithDefault (TVar n) n substs
    e -> embed e

  unsafeSubstitute = substitute

-- | A polymorphic type.
data Scheme
  -- | Universally quantified type variables.
  = TForall [Text] Type
  deriving (Eq, Show)

instance Substitutable Scheme where
  collectVars withVar withBinder (TForall names t) =
    foldMap withBinder names $ collectVars withVar withBinder t

  rename = runRenamer \badNames (TForall names t) ->
    uncurry TForall <$> replaceNames badNames names (pure t)

  -- I took a shot at implementing this but found it to be quite difficult
  -- because merging the foralls is tricky.
  -- It's not undoable, but it wasn't worth my further time investment
  -- seeing as this function isn't currently used anywhere.
  unsafeSubstitute = error "Substitution for schemes not yet implemented"

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

tapp :: [Type] -> Type
tapp []  = error "Empty type applications are not permitted"
tapp [t] = t
tapp ts  = foldl1' TApp ts

-- HACK
pattern TApp2 :: Type -> Type -> Type -> Type
pattern TApp2 tf tx ty = TApp (TApp tf tx) ty

-- TODO: Improve these printers.
unparseType :: Type -> Text
unparseType (TVar name) = name
unparseType (TApp2 TAbs a b) = "(" <> unparseType a <> " -> " <> unparseType b <> ")"
unparseType (TApp2 TProd a b) = "(" <> unparseType a <> " * " <> unparseType b <> ")"
unparseType (TApp2 TSum a b) = "(" <> unparseType a <> " + " <> unparseType b <> ")"
unparseType (TApp TList a) = "[" <> unparseType a <> "]"
unparseType (TApp a b) = "(" <> unparseType a <> " " <> unparseType b <> ")"
unparseType TAbs = "(->)"
unparseType TProd = "(*)"
unparseType TSum = "(+)"
unparseType TUnit = "★"
unparseType TVoid = "⊥"
unparseType TNat = "Nat"
unparseType TList = "[]"
unparseType TChar = "Char"

unparseScheme :: Scheme -> Text
unparseScheme (TForall [] t) = unparseType t
unparseScheme (TForall names t) = "∀" <> T.unwords names <>  ". " <> unparseType t
