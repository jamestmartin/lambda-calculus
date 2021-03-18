module LambdaCalculus.Types
  ( module LambdaCalculus.Types.Base
  , infer
  ) where

import LambdaCalculus.Types.Base

import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader (MonadReader, runReader, asks, local)
import Control.Monad.RWS
  ( RWST, evalRWST
  , MonadState, state
  , MonadWriter, tell, listen
  )
import Data.Foldable (forM_, toList)
import Data.HashSet qualified as HS
import Data.HashMap.Strict qualified as HM
import Data.Stream (Stream (..), fromList)
import Data.Text qualified as T

fresh :: MonadState (Stream Text) m => m Type
fresh = state \(Cons n ns) -> (TVar n, ns)

inst :: MonadState (Stream Text) m => Scheme -> m Type
inst (TForall ns t) = foldr (\n_n t' -> substitute1 n_n <$> fresh <*> t') (pure t) ns

lookupVar :: (MonadReader Context m, MonadState (Stream Text) m, MonadError Text m) => Text -> m Type
lookupVar n = do
  t_polyq <- asks (HM.!? n)
  case t_polyq of
    Nothing -> throwError $ "Variable not bound: " <> n
    Just t_poly -> inst t_poly

generalize :: MonadReader Context m => Type -> m Scheme
generalize t = do
  ctx <- asks HM.keysSet
  pure $ TForall (toList $ HS.difference (free t) ctx) t

bindVar :: MonadReader Context m => Text -> Type -> m a -> m a
bindVar n t = local (HM.insert n (TForall [] t))

unify :: MonadWriter [Constraint] m => Type -> Type -> m ()
unify t1 t2 = tell [(t1, t2)]

ctrTy :: MonadState (Stream Text) m => Ctr -> m (Type, [Type])
ctrTy = \case
  CUnit -> pure (TUnit, [])
  CZero -> pure (TNat, [])
  CSucc -> pure (TNat, [TNat])
  CChar -> pure (TChar, [TNat])
  CNil -> mkUnary TList $ const []
  CCons -> mkUnary TList \t_a -> [t_a, TApp TList t_a]
  CPair -> mkBinary TProd \t_a t_b -> [t_a, t_b]
  CLeft -> mkBinary TSum \t_a _ -> [t_a]
  CRight -> mkBinary TSum \_ t_b -> [t_b]
  where
    mkBinary tc tcas = do
      t_a <- fresh
      t_b <- fresh
      pure (tapp [tc, t_a, t_b], tcas t_a t_b)

    mkUnary tc tcas = do
      t_a <- fresh
      pure (TApp tc t_a, tcas t_a)

j :: (MonadError Text m, MonadReader Context m, MonadState (Stream Text) m, MonadWriter [Constraint] m)
  => CheckExpr -> m Type
j (Var name) = lookupVar name
j (App e_fun e_arg) = do
  t_ret <- fresh
  t_fun <- j e_fun
  t_arg <- j e_arg
  unify t_fun (tapp [TAbs, t_arg, t_ret])
  pure t_ret
j (Abs n_arg e_ret) = do
  t_arg <- fresh
  t_ret <- bindVar n_arg t_arg $ j e_ret
  pure $ tapp [TAbs, t_arg, t_ret]
j (Let (n_x, e_x) e_ret) = do
  (t_x_mono, c) <- listen $ j e_x
  s <- solve' c
  t_x_poly <- generalize $ substitute s t_x_mono
  local (HM.insert n_x t_x_poly) $ j e_ret
-- In a case expression:
--   * the pattern for each branch has the same type as the expression being matched, and
--   * the return type for each branch has the same type as the return type of the case expression as a whole.
j (Case ctrs) = do
  t_ret <- fresh
  t_x <- fresh
  forM_ ctrs \(Pat ctr ns_n e) -> do
    (t_x', ts_n) <- ctrTy ctr
    unify t_x t_x'
    when (length ts_n /= length ns_n) $ throwError "Constructor arity mismatch"
    t_ret' <- local (HM.union $ HM.fromList $ zip ns_n $ map (TForall []) ts_n) $ j e
    unify t_ret t_ret'
  pure $ tapp [TAbs, t_x, t_ret]
j (CtrC ctr) = do
  (t_ret, ts_n) <- ctrTy ctr
  pure $ foldr (\t_a t_r -> tapp [TAbs, t_a, t_r]) t_ret ts_n
j CallCCC = do
  t_a <- fresh
  t_b <- fresh
  pure $ tapp [TAbs, tapp [TAbs, tapp [TAbs, t_a, t_b], t_a], t_a]
j FixC = do
  t_a <- fresh
  pure $ tapp [TAbs, tapp [TAbs, t_a, t_a], t_a]
j HoleC = asks show >>= throwError . (<>) "Encountered hole with context: " . T.pack

occurs :: Text -> Type -> Bool
occurs n t = HS.member n (free t)

findDifference :: MonadError (Type, Type) m => Type -> Type -> m (Maybe (Text, Type))
findDifference t1 t2
  | t1 == t2 = pure Nothing
  | TVar n1 <- t1, not (occurs n1 t2) = pure $ Just (n1, t2)
  | TVar _ <- t2 = findDifference t2 t1
  | TApp a1 b1 <- t1, TApp a2 b2 <- t2 = (<|>) <$> findDifference a1 a2 <*> findDifference b1 b2
  | otherwise = throwError (t1, t2)

unifies :: MonadError (Type, Type) m => Type -> Type -> m Substitution
unifies t1 t2 = do
  dq <- findDifference t1 t2
  case dq of
    Nothing -> pure HM.empty
    Just s -> do
      ss <- unifies (uncurry substitute1 s t1) (uncurry substitute1 s t2)
      pure $ uncurry HM.insert (fmap (substitute ss) s) ss

solve :: MonadError (Type, Type) m => [Constraint] -> m Substitution
solve [] = pure HM.empty
solve (c:cs) = do
  s <- uncurry unifies c
  ss <- solve (substituteMono s cs)
  pure $ HM.union ss (substituteMono ss s)

solve' :: MonadError Text m => [Constraint] -> m Substitution
solve' c = case solve c of
  Right ss -> pure ss
  Left (t1, t2) -> throwError $ "Could not unify " <> unparseType t1 <> " with " <> unparseType t2

type Inferencer a = RWST Context [Constraint] (Stream Text) (Either Text) a

runInferencer :: Inferencer a -> Either Text (a, [Constraint])
runInferencer m = evalRWST m HM.empty freshNames
  where
    freshNames = fromList $ do
      n <- [0 :: Int ..]
      c <- ['a'..'z']
      pure $ T.pack if n == 0 then [c] else c : show n

infer :: CheckExpr -> Either Text Scheme
infer e = do
  (t, c) <- runInferencer $ j e
  s <- solve' c
  let t' = substitute s t
  pure $ runReader (generalize t') HM.empty
