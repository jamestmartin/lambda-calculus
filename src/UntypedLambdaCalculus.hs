{-# LANGUAGE GADTs, FlexibleInstances, DataKinds #-}
module UntypedLambdaCalculus (Expr (Free, Var, Lam, App), eval) where

import Control.Monad.Reader (Reader, runReader, withReader, reader, asks)
import Data.Fin (Fin (FZ, FS), extract, coerceFin)
import Data.Functor ((<&>))
import Data.Nat (Nat (S, Z))
import Data.Vec (Vec (Empty, (:.)), (!.))

-- | A lambda calculus expression where variables are identified
-- | by their distance from their binding site (De Bruijn indices).
data Expr n = Free String
            | Var (Fin n)
            | Lam String (Expr ('S n))
            | App (Expr n) (Expr n)

coerceExpr :: Expr n -> Expr ('S n)
coerceExpr (Free v)  = Free v
coerceExpr (Var v)   = Var $ coerceFin v
coerceExpr (Lam v e) = Lam v $ coerceExpr e
coerceExpr (App f x) = App (coerceExpr f) (coerceExpr x)

instance Show (Expr 'Z) where
  show expr = runReader (show' expr) Empty
    where show' :: Expr n -> Reader (Vec n String) String
          show' (Free v)    = return v
          show' (Var  v)    = reader (\vars -> vars !. v ++ ':' : show v)
          show' (Lam  v e') = withReader (v :.) (show' e') <&>
                           \body -> "(\\" ++ v ++ ". " ++ body ++ ")"
          show' (App f' x') = do
                             f <- show' f'
                             x <- show' x'
                             return $ "(" ++ f ++ " " ++ x ++ ")"

-- | Determine whether the variable bound by a lambda expression is used in its body.
-- | This is used in eta reduction, where `(\x. f x)` reduces to `f` when `x` is not bound in `f`.
unbound :: Expr ('S n) -> Bool
unbound expr = runReader (unbound' expr) FZ
  where unbound' :: Expr ('S n) -> Reader (Fin ('S n)) Bool
        unbound' (Free _)  = return True
        unbound' (Var v)   = reader (/= v)
        unbound' (App f x) = (&&) <$> unbound' f <*> unbound' x
        unbound' (Lam _ e) = withReader FS $ unbound' e

-- | When we bind a new variable, we enter a new scope.
-- | Since variables are identified by their distance from their binder,
-- | we must increment them to account for the incremented distance,
-- | thus embedding them into the new expression.
embed :: Expr n -> Expr ('S n)
embed   (Var v) = Var $ FS v
embed o@(Lam _ _) = coerceExpr o
embed   (Free x) = Free x
embed   (App f x) = App (embed f) (embed x)

subst :: Expr n -> Expr ('S n) -> Expr n
subst value expr = runReader (subst' value expr) FZ
  where subst' :: Expr n -> Expr ('S n) -> Reader (Fin ('S n)) (Expr n)
        subst' _   (Free x)  = return $ Free x
        subst' val (Var x)   = maybe val Var <$> asks (extract x)
        subst' val (App f x) = App <$> subst' val f <*> subst' val x
        subst' val (Lam v e) = Lam v <$> withReader FS (subst' (embed val) e)

-- | Evaluate an expression to normal form.
eval :: Expr n -> Expr n
eval (App f' x) = case eval f' of
  -- Beta reduction.
  Lam _ e -> eval $ subst x e
  f -> App f (eval x)
eval o@(Lam _ (App f (Var FZ)))
  -- Eta reduction. We know that `0` is not bound in `f`,
  -- so we can simply substitute it for undefined.
  | unbound f = eval $ subst undefined f
  | otherwise = o
eval o = o
