{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module UntypedLambdaCalculus (Expr (Free, Var, Lam, App), canonym, eval, normal, whnf) where

import Control.Applicative (liftA2)
import Control.Monad.Reader (Reader, runReader, ask, local, reader)
import Data.Function (fix)
import Data.Functor.Foldable (Base, Recursive, cata, embed, project)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.List (findIndex)
import UntypedLambdaCalculus.Parser (Ast (AstVar, AstLam, AstApp), AstF (AstVarF, AstLamF, AstAppF))

-- | Look up a recursion-schemes tutorial if you don't know what an Algebra means.
-- | I use recursion-schemes in this project a lot.
type Algebra f a = f a -> a

-- | A lambda calculus expression where variables are identified
-- | by their distance from their binding site (De Bruijn indices).
data Expr = Free String
          | Var Int
          | Lam String Expr
          | App Expr Expr

makeBaseFunctor ''Expr

instance Show Expr where
  show = cataReader alg []
    where alg :: Algebra ExprF (Reader [String] String)
          alg (FreeF v)   = return v
          alg (VarF  v)   = reader (\vars -> vars !! v ++ ':' : show v)
          alg (LamF  v e) = do
            body <- local (v :) e
            return $ "(\\" ++ v ++ ". " ++ body ++ ")"
          alg (AppF f' x') = do
            f <- f'
            x <- x'
            return $ "(" ++ f ++ " " ++ x ++ ")"

-- | Recursively reduce a `t` into an `a` when inner reductions are dependent on outer context.
-- | In other words, data flows outside-in, reductions flow inside-out.
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

-- | Convert an Ast into an Expression where all variables have canonical, unique names.
-- | Namely, bound variables are identified according to their distance from their binding site
-- | (i.e. De Bruijn indices).
canonym :: Ast -> Expr
canonym = cataReader alg []
  where alg :: Algebra AstF (Reader [String] Expr)
        alg (AstVarF v)   = maybe (Free v) Var <$> findIndex (== v) <$> ask
        alg (AstLamF v e) = Lam v <$> local (v :) e
        alg (AstAppF n m) = App <$> n <*> m



-- | When we bind a new variable, we enter a new scope.
-- | Since variables are identified by their distance from their binder,
-- | we must increment them to account for the incremented distance.
{-introduceBindingInExpr :: Expr -> Expr
introduceBindingInExpr = cataReader alg 0
  where alg :: Algebra ExprF (Reader Int Expr)
        alg (VarF v) = reader $ \x ->
          if v > x then Var $ v + 1 else Var v
        alg (LamF v e) = Lam v <$> local (+ 1) e
        alg (AppF f x) = App <$> f <*> x
        alg (FreeF v)  = return $ Free v-}
introduceBindingInExpr :: Expr -> Expr
introduceBindingInExpr (Var v) = Var $ v + 1
introduceBindingInExpr o@(Lam _ _) = o
introduceBindingInExpr x = embed $ fmap introduceBindingInExpr $ project x

introduceBinding :: Expr -> Reader [Expr] a -> Reader [Expr] a
introduceBinding x = local (\exprs -> x : map introduceBindingInExpr exprs)

intoBinding :: Reader [Expr] a -> Reader [Expr] a
intoBinding = introduceBinding (Var 0)

intoEta :: Reader [Expr] a -> Reader [Expr] a
intoEta = introduceBinding undefined

-- | Substitute all bound variables in an expression for their values,
-- | without performing any further evaluation.
subst :: Expr -> Reader [Expr] Expr
subst = cata alg
  where alg :: Algebra ExprF (Reader [Expr] Expr)
        alg (VarF v)   = reader (!! v)
        alg (AppF f x) = App <$> f <*> x
        alg (FreeF x)  = return $ Free x
        -- In a lambda expression, we substitute the parameter with itself.
        -- The rest of the substitutions may reference variables outside this binding,
        -- so that (Var 0) would refer not to this lambda, but the lambda outside it.
        -- Thus, we must increment all variables in the expression to be substituted in.
        alg (LamF v e) = Lam v <$> intoBinding e

-- | Evaluate a variable to normal form.
eval :: Expr -> Expr
eval expr = runReader (eval' expr) []
  where eval' (App f' x') = do
          f <- eval' f'
          x <- eval' x'
          case f of
            Lam _ e -> introduceBinding x $ eval' e
            _ -> return $ App f x
        eval' o@(Lam _ (App f (Var 0)))
          | unbound f = intoEta $ eval' f
          | otherwise = subst o
        eval' x = subst x

-- | Is an expression in normal form?
normal :: Expr -> Bool
normal (App (Lam _ _) _) = False
normal (Lam _ (App f (Var 0))) = unbound f
normal (App f x) = normal f && normal x
normal _ = True

-- | Is an expression in weak head normal form?
whnf :: Expr -> Bool
whnf (App (Lam _ _) _) = False
whnf (Lam _ (App f (Var 0))) = unbound f
whnf (App f _) = whnf f
whnf _ = True
