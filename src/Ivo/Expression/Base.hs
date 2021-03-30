{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Ivo.Expression.Base
  ( Text, VoidF, UnitF (..), absurd'
  , Expr (..), Ctr (..), Pat, Def, AppArgs, AbsArgs, LetArgs, CtrArgs, AnnX, XExpr
  , ExprF (..), PatF (..), DefF (..), AppArgsF, LetArgsF, CtrArgsF, XExprF
  , Type (..), TypeF (..), Scheme (..), tapp
  , RecursivePhase, projectAppArgs, projectLetArgs, projectCtrArgs, projectXExpr, projectDef
  , embedAppArgs, embedLetArgs, embedCtrArgs, embedXExpr, embedDef
  , Substitutable, free, freeIn, bound, used, collectVars, rename, rename1
  , substitute, substitute1, unsafeSubstitute, unsafeSubstitute1
  , runRenamer, freshVar, replaceNames, runSubstituter, maySubstitute
  , compose, composeMap
  ) where

import Control.Monad.Reader (MonadReader, Reader, runReader, asks, local)
import Control.Monad.State (MonadState, StateT, evalStateT, state)
import Control.Monad.Zip (MonadZip, mzipWith)
import Data.Foldable (fold)
import Data.Functor.Foldable (Base, Recursive, Corecursive, project, embed, cata)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.Kind qualified as Kind
import Data.List (foldl1')
import Data.Stream qualified as S
import Data.Text (Text)
import Data.Text qualified as T

data Expr phase
  -- | A variable: @x@.
  = Var !Text
  -- | Function application: @f x_0 ... x_n@.
  | App !(Expr phase) !(AppArgs phase)
  -- | Lambda abstraction: @λx_0 ... x_n. e@.
  | Abs !(AbsArgs phase) !(Expr phase)
  -- | Let expression: @let x_0 = v_0 ... ; x_n = v_n in e@.
  | Let !(LetArgs phase) !(Expr phase)
  -- | Data constructor, e.g. @(x, y)@ or @Left@.
  | Ctr !Ctr !(CtrArgs phase)
  -- | Case expression to pattern match against a value,
  -- e.g. @case { Left x1 -> e1 ; Right x2 -> e2 }@.
  | Case ![Pat phase]
  -- | Type annotations: @expr : type@.
  | Ann !(AnnX phase) !(Expr phase) Type
  -- | Additional phase-specific constructors.
  | ExprX !(XExpr phase)

type family AppArgs phase
type family AbsArgs phase
type family LetArgs phase
type family CtrArgs phase
type family AnnX    phase
type family XExpr phase

class
  ( c (AppArgs phase)
  , c (AbsArgs phase)
  , c (LetArgs phase)
  , c (CtrArgs phase)
  , c (AnnX    phase)
  , c (XExpr   phase)
  ) => ForallX c phase

instance
  ( c (AppArgs phase)
  , c (AbsArgs phase)
  , c (LetArgs phase)
  , c (CtrArgs phase)
  , c (AnnX    phase)
  , c (XExpr   phase)
  ) => ForallX c phase

deriving instance ForallX Eq phase  => Eq (Expr phase)

deriving instance ForallX Show phase => Show (Expr phase)

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

tapp :: [Type] -> Type
tapp []  = error "Empty type applications are not permitted"
tapp [t] = t
tapp ts  = foldl1' TApp ts

-- | A polymorphic type.
data Scheme
  -- | Universally quantified type variables.
  = TForall [Text] Type
  deriving (Eq, Show)

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
  | AnnF !(AnnX phase) r Type
  | ExprXF !(XExprF phase r)

type instance Base (Expr phase) = ExprF phase

type family AppArgsF phase :: Kind.Type -> Kind.Type
type family LetArgsF phase :: Kind.Type -> Kind.Type
type family CtrArgsF phase :: Kind.Type -> Kind.Type
type family XExprF   phase :: Kind.Type -> Kind.Type

class
  ( c (AppArgsF phase)
  , c (LetArgsF phase)
  , c (CtrArgsF phase)
  , c (XExprF   phase)
  ) => ForallXF c phase

instance
  ( c (AppArgsF phase)
  , c (LetArgsF phase)
  , c (CtrArgsF phase)
  , c (XExprF   phase)
  ) => ForallXF c phase

data DefF r = Def !Text !r
  deriving (Eq, Functor, Foldable, Traversable, Show)

-- | A contractible data type with one extra type parameter.
data UnitF a = Unit
  deriving (Eq, Functor, Foldable, Traversable, Show)

-- | An empty type with one extra type parameter.
data VoidF a
  deriving (Eq, Functor, Foldable, Traversable, Show)

absurd' :: VoidF a -> b
absurd' x = case x of {}

instance ForallXF Functor phase  => Functor (ExprF phase) where
  fmap f = \case
    VarF n -> VarF n
    AppF ef exs -> AppF (f ef) (fmap f exs)
    AbsF ns e -> AbsF ns (f e)
    LetF ds e -> LetF (fmap f ds) (f e)
    CtrF c es -> CtrF c (fmap f es)
    CaseF ps -> CaseF (fmap (fmap f) ps)
    AnnF x e t -> AnnF x (f e) t
    ExprXF q -> ExprXF (fmap f q)

instance ForallXF Foldable phase => Foldable (ExprF phase) where
  foldMap f = \case
    VarF _ -> mempty
    AppF ef exs -> f ef <> foldMap f exs
    AbsF _ e -> f e
    LetF ds e -> foldMap f ds <> f e
    CtrF _ es -> foldMap f es
    CaseF ps -> foldMap (foldMap f) ps
    AnnF _ e _ -> f e
    ExprXF q -> foldMap f q

instance ForallXF Traversable phase => Traversable (ExprF phase) where
  traverse f = \case
    VarF n -> pure $ VarF n
    AppF ef exs -> AppF <$> f ef <*> traverse f exs
    AbsF ns e -> AbsF ns <$> f e
    LetF ds e -> LetF <$> traverse f ds <*> f e
    CtrF c es -> CtrF c <$> traverse f es
    CaseF ps -> CaseF <$> traverse (traverse f) ps
    AnnF x e t -> (\e' -> AnnF x e' t) <$> f e
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
projectDef = uncurry Def

embedDef :: DefF (Expr phase) -> Def phase
embedDef (Def n e) = (n, e)

instance RecursivePhase phase => Recursive (Expr phase) where
  project = \case
    Var n -> VarF n
    App ef exs -> AppF ef (projectAppArgs exs)
    Abs ns e -> AbsF ns e
    Let ds e -> LetF (projectLetArgs ds) e
    Ctr c es -> CtrF c (projectCtrArgs es)
    Case ps -> CaseF ps
    Ann x e t -> AnnF x e t
    ExprX q -> ExprXF (projectXExpr q)

instance RecursivePhase phase => Corecursive (Expr phase) where
  embed = \case
    VarF n -> Var n
    AppF ef exs -> App ef (embedAppArgs exs)
    AbsF ns e -> Abs ns e
    LetF ds e -> Let (embedLetArgs ds) e
    CtrF c es -> Ctr c (embedCtrArgs es)
    CaseF ps -> Case ps
    AnnF x e t -> Ann x e t
    ExprXF q -> ExprX (embedXExpr q)

makeBaseFunctor ''Type

---
--- End base functor boilerplate.
---

class Substitutable e where
  -- | Fold over the variables in the expression with a monoid,
  -- given what to do with variable usage sites and binding sites respectively.
  collectVars :: Monoid m => (Text -> m) -> (Text -> m -> m) -> e -> m

  -- | Free variables are variables which occur anywhere in an expression
  -- where they are not bound by an abstraction.
  free :: e -> HashSet Text
  free = collectVars HS.singleton HS.delete

  -- | Is the given name a member of the expression's free variables?
  freeIn :: Text -> e -> Bool
  freeIn name = HS.member name . free

  -- | Bound variables are variables which are abstracted over anywhere in an expression.
  bound :: e -> HashSet Text
  bound = collectVars (const HS.empty) HS.insert

  -- | Used variables are variables which appear *anywhere* in an expression, free or bound.
  used :: e -> HashSet Text
  used = collectVars HS.singleton HS.insert

  -- | Given a map between variable names and expressions,
  -- replace each free occurrence of a variable with its respective expression.
  substitute :: HashMap Text e -> e -> e
  substitute substs = unsafeSubstitute substs . rename (foldMap free substs)

  substitute1 :: Text -> e -> e -> e
  substitute1 n e = substitute (HM.singleton n e)

  -- | Rename all bound variables in an expression (both a binding sites and usage sites)
  -- with new names where the new names are *not* members of the provided set.
  rename :: HashSet Text -> e -> e

  rename1 :: Text -> e -> e
  rename1 n = rename (HS.singleton n)

  -- | A variant of substitution which does *not* avoid variable capture;
  -- it only gives the correct result if the bound variables in the body
  -- are disjoint from the free variables in the argument.
  unsafeSubstitute :: HashMap Text e -> e -> e

  unsafeSubstitute1 :: Text -> e -> e -> e
  unsafeSubstitute1 n e = unsafeSubstitute (HM.singleton n e)

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

--
-- These primitives are likely to be useful for implementing `rename`.
-- Ideally, I would like to find a way to move the implementation of `rename` here entirely,
-- but I haven't yet figured out an appropriate abstraction to do so.
--

-- | Run an action which requires a stateful context of used variable names
-- and a local context of variable replacements.
--
-- This is a useful monad for implementing the `rename` function.
runRenamer :: Substitutable e
           => (HashSet Text -> e -> StateT (HashSet Text) (Reader (HashMap Text Text)) a)
           -> HashSet Text
           -> e
           -> a
runRenamer m ctx e = runReader (evalStateT (m ctx e) dirtyNames) HM.empty
  where dirtyNames = HS.union ctx (used e)

-- | Create a new variable name within a context of used variable names.
freshVar :: MonadState (HashSet Text) m => Text -> m Text
freshVar baseName =
  state \ctx -> let name = newName ctx in (name, HS.insert name ctx)
  where
    names = S.iterate (`T.snoc` '\'') baseName
    newName ctx = S.head $ S.filter (not . (`HS.member` ctx)) names

-- | Replace a collection of old variable names with new variable names
-- and apply those replacements within a context.
replaceNames :: ( MonadReader (HashMap Text Text) m
                , MonadState  (HashSet Text)      m
                , MonadZip t, Traversable t
                )
              => HashSet Text -> t Text -> m a -> m (t Text, a)
replaceNames badNames names m = do
  newNames <- mapM freshVarIfNecessary names
  let replacements = HM.filterWithKey (/=) $ fold $ mzipWith HM.singleton names newNames
  x <- local (HM.union replacements) m
  pure (newNames, x)
  where
    freshVarIfNecessary name
      | name `HS.member` badNames = freshVar name
      | otherwise = pure name

---
--- The same as the above section but for `substitute`.
--- This is useful when implementing substitution as a paramorphism.
---

-- | Run an action in a local context of substitutions.
--
-- This monad is useful for implementing, you guessed it, substitution.
runSubstituter :: (e -> Reader (HashMap Text e) a)
               -> HashMap Text e
               -> e
               -> a
runSubstituter m substs e = runReader (m e) substs

-- | Apply only the substitutions which are not bound,
-- and only if there are substitutions left to apply.
maySubstitute :: ( MonadReader (HashMap Text b) m
                 , Functor t, Foldable t
                 )
              => t Text -> (a, m a) -> m a
maySubstitute ns (unmodified, substituted) =
  local (composeMap HM.delete ns) do
    noMoreSubsts <- asks HM.null
    if noMoreSubsts
      then pure unmodified
      else substituted

compose :: Foldable t => t (a -> a) -> (a -> a)
compose = foldr (.) id

composeMap :: (Functor t, Foldable t) => (a -> (b -> b)) -> t a -> (b -> b)
composeMap f = compose . fmap f
