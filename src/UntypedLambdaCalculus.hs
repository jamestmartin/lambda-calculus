{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module UntypedLambdaCalculus where

import Control.Applicative (liftA2)
import Control.Monad.Reader (Reader, runReader, ask, local, reader)
import Data.Function (fix)
import Data.Functor.Foldable (Base, Recursive, cata, embed)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.HashSet (HashSet, empty, singleton, union, member, delete)
import Data.List (findIndex)
import UntypedLambdaCalculus.Parser (Ast (AstVar, AstLam, AstApp), AstF (AstVarF, AstLamF, AstAppF))

type Algebra f a = f a -> a

data Expr = Free String
          | Var Int
          | Lam String Expr
          | App Expr Expr

makeBaseFunctor ''Expr

-- | Recursively reduce a `t` into an `a` when inner reductions are dependent on outer context.
cataReader :: Recursive t => Algebra (Base t) (Reader s a) -> s -> t -> a
cataReader alg s x = runReader (cata alg x) s

-- | Determine whether the variable bound by a lambda expression is used in its body.
-- | This is used in eta reduction, where `(\x. f x)` reduces to `f` when `x` is not bound in `f`.
unbound :: Expr -> Bool
unbound = cataReader alg 0
  where alg :: Algebra ExprF (Reader Int Bool)
        alg (FreeF _)  = return True
        alg (VarF v)   = reader (/= v)
        alg (AppF f x) = (&&) <$> f <*> x
        alg (LamF _ e) = local (+ 1) e

eval :: [Expr] -> Expr -> Expr
eval env (Var v) = env !! v
eval env (App f' x') = case f of
  Lam _ e -> eval (x : env) e
  _ -> App f x
  where f = eval env f'
        x = eval env x'
eval env o@(Lam _ (App f (Var 0)))
  | unbound f = eval (undefined : env) f
  | otherwise = o
eval env x = runReader (substExpr x) env

free :: Ast -> HashSet String
free = cata alg
  where alg :: Algebra AstF (HashSet String)
        alg (AstVarF x)   = singleton x
        alg (AstLamF x m) = delete x m
        alg (AstAppF m n) = m `union` n

canonym :: Ast -> Expr
canonym = cataReader alg []
  where alg :: Algebra AstF (Reader [String] Expr)
        alg (AstVarF v)    = maybe (Free v) Var <$> findIndex (== v) <$> ask
        alg (AstLamF v e') = Lam v <$> local (v :) e'
        alg (AstAppF n m)  = App <$> n <*> m

rename :: String -> HashSet String -> String
rename var free
  -- Continue prepending `_` until the name is free.
  | var `member` free = rename newVar free
  | otherwise = var
  where newVar = '_' : var

subst :: String -> Ast -> Ast -> Ast
subst var n o@(AstVar var')
  | var == var' = n
  | otherwise   = o
subst var n (AstApp m1 m2) = AstApp (subst var n m1) (subst var n m2)
subst var n o@(AstLam var' m)
  | var == var' = o
  -- Alpha-convert as necessary, and then substitute into the body.
  | otherwise = AstLam newVar $ subst var n $ subst var' (AstVar newVar) m
  where newVar = rename var' $ free n

instance Show Expr where
  show = cataReader alg []
    where alg :: Algebra ExprF (Reader [String] String)
          alg (FreeF v)   = return v
          alg (VarF  v)   = reader (\vars -> vars !! v ++ show v)
          alg (LamF  v e) = do
            body <- local (v :) e
            return $ "(\\" ++ v ++ ". " ++ body ++ ")"
          alg (AppF f' x') = do
            f <- f'
            x <- x'
            return $ "(" ++ f ++ " " ++ x ++ ")"

type Eval' = Expr -> Reader [Expr] Expr

incrementVars :: Expr -> Expr
incrementVars = cata alg
  where alg (VarF x) = Var $ x + 1
        alg x = embed x

substExpr :: Eval'
substExpr = cata alg
  where alg :: Algebra ExprF (Reader [Expr] Expr)
        alg (VarF v)   = reader (!! v)
        alg (AppF f x) = App <$> f <*> x
        alg (FreeF x)  = return $ Free x
        alg (LamF v e) = Lam v <$> local (\exprs -> Var 0 : map incrementVars exprs) e
