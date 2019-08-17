{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveFunctor, DeriveFoldable, DeriveTraversable, FlexibleInstances, MultiParamTypeClasses #-}
module UntypedLambdaCalculus (Expr (Free, Var, Lam, App), eval) where

import Control.Monad.Reader (Reader, runReader, withReader, reader, asks)
import Data.Fin (Fin (Zero, Succ), finRemove)
import Data.Injection (Injection, inject)
import Data.Type.Nat (Nat, Succ, Zero)
import Data.Vec (Vec (Empty, (:.)), (!.))

-- | A lambda calculus expression where variables are identified
-- | by their distance from their binding site (De Bruijn indices).
data Expr n = Free String
            | Var (Fin n)
            | Lam String (Expr (Succ n))
            | App (Expr n) (Expr n)

instance (Nat n) => Injection (Expr n) (Expr (Succ n)) where
  inject (Free v)  = Free v
  inject (Var v)   = Var $ inject v
  inject (Lam v e) = Lam v $ inject e
  inject (App f x) = App (inject f) (inject x)

instance Show (Expr Zero) where
  show expr = runReader (alg expr) Empty
    where alg :: Nat n => Expr n -> Reader (Vec n String) String
          alg (Free v)   = return v
          alg (Var  v)   = reader (\vars -> vars !. v ++ ':' : show v)
          alg (Lam  v e) = do
            body <- withReader (v :.) $ alg e
            return $ "(\\" ++ v ++ ". " ++ body ++ ")"
          alg (App f' x') = do
            f <- alg f'
            x <- alg x'
            return $ "(" ++ f ++ " " ++ x ++ ")"

-- | Determine whether the variable bound by a lambda expression is used in its body.
-- | This is used in eta reduction, where `(\x. f x)` reduces to `f` when `x` is not bound in `f`.
unbound :: Nat n => Expr (Succ n) -> Bool
unbound expr = runReader (alg expr) Zero
  where alg :: Nat n => Expr (Succ n) -> Reader (Fin (Succ n)) Bool
        alg (Free _)  = return True
        alg (Var v)   = reader (/= v)
        alg (App f x) = (&&) <$> alg f <*> alg x
        alg (Lam _ e) = withReader Succ $ alg e

-- | When we bind a new variable, we enter a new scope.
-- | Since variables are identified by their distance from their binder,
-- | we must increment them to account for the incremented distance,
-- | thus embedding them into the new expression.
embed' :: Nat n => Expr n -> Expr (Succ n)
embed' (Var v) = Var $ Succ v
embed' o@(Lam _ _) = inject o
embed' (Free x) = Free x
embed' (App f x) = App (embed' f) (embed' x)

subst :: Nat n => Expr n -> Expr (Succ n) -> Expr n
subst value expr = runReader (subst' value expr) Zero
  where subst' :: Nat n => Expr n -> Expr (Succ n) -> Reader (Fin (Succ n)) (Expr n)
        subst' _   (Free x)  = return $ Free x
        subst' val (Var x)   = maybe val Var <$> asks (finRemove x)
        subst' val (App f x) = App <$> subst' val f <*> subst' val x
        subst' val (Lam v e) = Lam v <$> withReader Succ (subst' (embed' val) e)

-- | Evaluate an expression to normal form.
eval :: Nat n => Expr n -> Expr n
eval (App f' x) = case eval f' of
  -- Beta reduction.
  Lam _ e -> eval $ subst x e
  f -> App f (eval x)
eval o@(Lam _ (App f (Var Zero)))
  -- Eta reduction. We know that `0` is not bound in `f`,
  -- so we can simply substitute it for undefined.
  | unbound f = eval $ subst undefined f
  | otherwise = o
eval o = o
