{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveFunctor, DeriveFoldable, DeriveTraversable, FlexibleInstances #-}
module UntypedLambdaCalculus (Expr (Free, Var, Lam, App), canonym, eval, normal, whnf) where

import Control.Applicative (liftA2)
import Control.Monad.Reader (Reader, runReader, ask, local, withReader, reader, asks)
import Data.Fin (Fin (Zero, Succ), finUp, finRemove)
import Data.Function (fix)
import Data.Functor.Foldable (Base, Recursive, Corecursive, ListF (Nil, Cons), cata, embed, project)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Injection (inject)
import Data.Maybe (fromJust)
import Data.Type.Nat (Nat, Succ, Zero)
import Data.Vec (Vec (Empty, (:.)), (!.), vmap, elemIndexVec)
import UntypedLambdaCalculus.Parser (Ast (AstVar, AstLam, AstApp))

type Algebra f a = f a -> a

-- | A lambda calculus expression where variables are identified
-- | by their distance from their binding site (De Bruijn indices).
data Expr n = Free String
            | Var (Fin n)
            | Lam String (Expr (Succ n))
            | App (Expr n) (Expr n)

makeBaseFunctor ''Expr

exprUp :: Nat n => Expr n -> Expr (Succ n)
exprUp (Free v) = Free v
exprUp (Var v) = Var $ finUp v
exprUp (Lam v e) = Lam v $ exprUp e
exprUp (App f x) = App (exprUp f) (exprUp x)

instance Show (Expr Zero) where
  show x = runReader (alg x) Empty
    where alg :: Nat n => Expr n -> Reader (Vec String n) String
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
unbound x = runReader (alg x) Zero
  where alg :: Nat n => Expr (Succ n) -> Reader (Fin (Succ n)) Bool
        alg (Free _)  = return True
        alg (Var v)   = reader (/= v)
        alg (App f x) = (&&) <$> alg f <*> alg x
        alg (Lam _ e) = withReader Succ $ alg e

-- | Convert an Ast into an Expression where all variables have canonical, unique names.
-- | Namely, bound variables are identified according to their distance from their binding site
-- | (i.e. De Bruijn indices).
canonym :: Ast -> Expr Zero
canonym x = runReader (alg x) Empty
  where alg :: Nat n => Ast -> Reader (Vec String n) (Expr n)
        alg (AstVar v)   = maybe (Free v) Var <$> elemIndexVec v <$> ask
        alg (AstLam v e) = Lam v <$> withReader (v :.) (alg e)
        alg (AstApp n m) = App <$> alg n <*> alg m

-- | When we bind a new variable, we enter a new scope.
-- | Since variables are identified by their distance from their binder,
-- | we must increment them to account for the incremented distance.
introduceBindingInExpr :: Nat n => Expr n -> Expr (Succ n)
introduceBindingInExpr (Var v) = Var $ Succ v
introduceBindingInExpr o@(Lam _ _) = exprUp o
introduceBindingInExpr (Free x) = Free x
introduceBindingInExpr (App f x) = App (introduceBindingInExpr f) (introduceBindingInExpr x)

intoEta :: Nat n => Expr (Succ n) -> Expr n
intoEta x = runReader (intoEta' x) Zero
  where intoEta' :: Nat n => Expr (Succ n) -> Reader (Fin (Succ n)) (Expr n)
        intoEta' (Free x) = return $ Free x
        intoEta' (Var x) = Var <$> fromJust <$> asks (finRemove x)
        intoEta' (App f x) = App <$> intoEta' f <*> intoEta' x
        intoEta' (Lam v e) = Lam v <$> withReader Succ (intoEta' e)

subst :: Nat n => Expr n -> Expr (Succ n) -> Expr n
subst val x = runReader (subst' val x) Zero
  where subst' :: Nat n => Expr n -> Expr (Succ n) -> Reader (Fin (Succ n)) (Expr n)
        subst' _   (Free x)  = return $ Free x
        subst' val (Var x)   = maybe val Var <$> asks (finRemove x)
        subst' val (App f x) = App <$> subst' val f <*> subst' val x
        subst' val (Lam v e) = Lam v <$> withReader Succ (subst' (introduceBindingInExpr val) e)

-- | Evaluate a variable to normal form.
eval :: Nat n => Expr n -> Expr n
eval (App f' x) = case eval f' of
  Lam _ e -> eval $ subst x e
  f -> App f (eval x)
eval o@(Lam _ (App f (Var Zero)))
  | unbound f = eval $ intoEta f
  | otherwise = o
eval o = o

-- | Is an expression in normal form?
normal :: Nat n => Expr n -> Bool
normal (App (Lam _ _) _) = False
normal (Lam _ (App f (Var Zero))) = unbound f
normal (App f x) = normal f && normal x
normal _ = True

-- | Is an expression in weak head normal form?
whnf :: Nat n => Expr n -> Bool
whnf (App (Lam _ _) _) = False
whnf (Lam _ (App f (Var Zero))) = unbound f
whnf (App f _) = whnf f
whnf _ = True
