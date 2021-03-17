{-# LANGUAGE UndecidableInstances #-}
module LambdaCalculus.Expression.Base
  ( Text, VoidF, UnitF (..), absurd'
  , Expr (..), Ctr (..), Pat, Def, AppArgs, AbsArgs, LetArgs, CtrArgs, XExpr
  , ExprF (..), PatF (..), DefF (..), AppArgsF, LetArgsF, CtrArgsF, XExprF
  , RecursivePhase, projectAppArgs, projectLetArgs, projectCtrArgs, projectXExpr, projectDef
  , embedAppArgs, embedLetArgs, embedCtrArgs, embedXExpr, embedDef
  ) where

import Data.Functor.Foldable (Base, Recursive, Corecursive, project, embed)
import Data.Kind (Type)
import Data.Text (Text)

data Expr phase
  -- | A variable: `x`.
  = Var !Text
  -- | Function application: `f x_0 ... x_n`.
  | App !(Expr phase) !(AppArgs phase)
  -- | Lambda abstraction: `λx_0 ... x_n. e`.
  | Abs !(AbsArgs phase) !(Expr phase)
  -- | Let expression: `let x_0 = v_0 ... ; x_n = v_n in e`.
  | Let !(LetArgs phase) !(Expr phase)
  -- | Data constructor, e.g. `(x, y)` or `Left`.
  | Ctr !Ctr !(CtrArgs phase)
  -- | Case expression to pattern match against a value,
  -- e.g. `case { Left x1 -> e1 ; Right x2 -> e2 }`.
  | Case ![Pat phase]
  -- | Additional phase-specific constructors.
  | ExprX !(XExpr phase)

type family AppArgs phase
type family AbsArgs phase
type family LetArgs phase
type family CtrArgs phase
type family XExpr phase

deriving instance
  ( Eq (AppArgs phase)
  , Eq (AbsArgs phase)
  , Eq (LetArgs phase)
  , Eq (CtrArgs phase)
  , Eq (XExpr   phase)
  ) => Eq (Expr phase)

deriving instance
  ( Show (AppArgs phase)
  , Show (AbsArgs phase)
  , Show (LetArgs phase)
  , Show (CtrArgs phase)
  , Show (XExpr   phase)
  ) => Show (Expr phase)

-- | Data constructors (used in pattern matching and literals).
data Ctr
  -- | `() : ★`
  = CUnit
  -- | `(x : a, y : b) : a * b`
  | CPair
  -- | `Left (x : a) : forall b. a + b`
  | CLeft
  -- | `Right (x : b) : forall a. a + b`
  | CRight
  -- | `0 : Nat`
  | CZero
  -- | `1+ (n : Nat) : Nat`
  | CSucc
  -- | `[] : forall a. List a`
  | CNil
  -- | `(x : a) :: (xs : List a) : List a`
  | CCons
  -- | `Char :: Nat -> Char`
  | CChar
  deriving (Eq, Show)

-- | A single pattern of a case expression, e.g. `(x, y) -> e`.
type Pat phase = PatF (Expr phase)
data PatF r = Pat { patCtr :: !Ctr, patNames :: ![Text], patBody :: !r }
  deriving (Eq, Functor, Foldable, Traversable, Show)

-- | A definition, mapping a name to a value.
type Def phase = (Text, Expr phase)

---
--- Base functor boilerplate for recursion-schemes
---

data ExprF phase r
  = VarF !Text
  | AppF !r !(AppArgsF phase r)
  | AbsF !(AbsArgs phase) r
  | LetF !(LetArgsF phase r) r
  | CtrF Ctr (CtrArgsF phase r)
  | CaseF [PatF r]
  | ExprXF !(XExprF phase r)

type instance Base (Expr phase) = ExprF phase

type family AppArgsF phase :: Type -> Type
type family LetArgsF phase :: Type -> Type
type family CtrArgsF phase :: Type -> Type
type family XExprF phase :: Type -> Type

data DefF r = DefF !Text !r
  deriving (Eq, Functor, Show)

-- | A contractible data type with one extra type parameter.
data UnitF a = Unit
  deriving (Eq, Functor, Foldable, Traversable, Show)

-- | An empty type with one extra type parameter.
data VoidF a
  deriving (Eq, Functor, Foldable, Traversable, Show)

absurd' :: VoidF a -> b
absurd' x = case x of {}

instance
  ( Functor (AppArgsF phase)
  , Functor (LetArgsF phase)
  , Functor (CtrArgsF phase)
  , Functor (XExprF phase)
  ) => Functor (ExprF phase) where
  fmap f = \case
    VarF n -> VarF n
    AppF ef exs -> AppF (f ef) (fmap f exs)
    AbsF ns e -> AbsF ns (f e)
    LetF ds e -> LetF (fmap f ds) (f e)
    CtrF c es -> CtrF c (fmap f es)
    CaseF ps -> CaseF (fmap (fmap f) ps)
    ExprXF q -> ExprXF (fmap f q)

instance
  ( Foldable (AppArgsF phase)
  , Foldable (LetArgsF phase)
  , Foldable (CtrArgsF phase)
  , Foldable (XExprF   phase)
  ) => Foldable (ExprF phase) where
  foldMap f = \case
    VarF _ -> mempty
    AppF ef exs -> f ef <> foldMap f exs
    AbsF _ e -> f e
    LetF ds e -> foldMap f ds <> f e
    CtrF _ es -> foldMap f es
    CaseF ps -> foldMap (foldMap f) ps
    ExprXF q -> foldMap f q

instance
  ( Traversable (AppArgsF phase)
  , Traversable (LetArgsF phase)
  , Traversable (CtrArgsF phase)
  , Traversable (XExprF   phase)
  ) => Traversable (ExprF phase) where
  traverse f = \case
    VarF n -> pure $ VarF n
    AppF ef exs -> AppF <$> f ef <*> traverse f exs
    AbsF ns e -> AbsF ns <$> f e
    LetF ds e -> LetF <$> traverse f ds <*> f e
    CtrF c es -> CtrF c <$> traverse f es
    CaseF ps -> CaseF <$> traverse (traverse f) ps
    ExprXF q -> ExprXF <$> traverse f q

class Functor (ExprF phase) => RecursivePhase phase where
  projectAppArgs :: AppArgs phase -> AppArgsF phase (Expr phase)
  projectLetArgs :: LetArgs phase -> LetArgsF phase (Expr phase)
  projectCtrArgs :: CtrArgs phase -> CtrArgsF phase (Expr phase)
  projectXExpr   :: XExpr   phase -> XExprF   phase (Expr phase)

  embedAppArgs :: AppArgsF phase (Expr phase) -> AppArgs phase
  embedLetArgs :: LetArgsF phase (Expr phase) -> LetArgs phase
  embedCtrArgs :: CtrArgsF phase (Expr phase) -> CtrArgs phase
  embedXExpr   :: XExprF   phase (Expr phase) -> XExpr   phase

  default projectAppArgs :: AppArgs phase ~  AppArgsF phase (Expr phase)
                         => AppArgs phase -> AppArgsF phase (Expr phase)
  default projectLetArgs :: LetArgs phase ~  LetArgsF phase (Expr phase)
                         => LetArgs phase -> LetArgsF phase (Expr phase)
  default projectCtrArgs :: CtrArgs phase ~  CtrArgsF phase (Expr phase)
                         => CtrArgs phase -> CtrArgsF phase (Expr phase)
  default projectXExpr   :: XExpr   phase ~  XExprF   phase (Expr phase)
                         => XExpr   phase -> XExprF   phase (Expr phase)

  default embedAppArgs :: AppArgsF phase (Expr phase) ~  AppArgs phase
                       => AppArgsF phase (Expr phase) -> AppArgs phase
  default embedLetArgs :: LetArgsF phase (Expr phase) ~  LetArgs phase
                       => LetArgsF phase (Expr phase) -> LetArgs phase
  default embedCtrArgs :: CtrArgsF phase (Expr phase) ~  CtrArgs phase
                       => CtrArgsF phase (Expr phase) -> CtrArgs phase
  default embedXExpr   :: XExprF   phase (Expr phase) ~  XExpr   phase
                       => XExprF   phase (Expr phase) -> XExpr   phase

  projectAppArgs = id
  projectLetArgs = id
  projectCtrArgs = id
  projectXExpr   = id

  embedAppArgs = id
  embedLetArgs = id
  embedCtrArgs = id
  embedXExpr   = id

projectDef :: Def phase -> DefF (Expr phase)
projectDef = uncurry DefF

embedDef :: DefF (Expr phase) -> Def phase
embedDef (DefF n e) = (n, e)

instance RecursivePhase phase => Recursive (Expr phase) where
  project = \case
    Var n -> VarF n
    App ef exs -> AppF ef (projectAppArgs exs)
    Abs ns e -> AbsF ns e
    Let ds e -> LetF (projectLetArgs ds) e
    Ctr c es -> CtrF c (projectCtrArgs es)
    Case ps -> CaseF ps
    ExprX q -> ExprXF (projectXExpr q)

instance RecursivePhase phase => Corecursive (Expr phase) where
  embed = \case
    VarF n -> Var n
    AppF ef exs -> App ef (embedAppArgs exs)
    AbsF ns e -> Abs ns e
    LetF ds e -> Let (embedLetArgs ds) e
    CtrF c es -> Ctr c (embedCtrArgs es)
    CaseF ps -> Case ps
    ExprXF q -> ExprX (embedXExpr q)

---
--- End base functor boilerplate.
---
