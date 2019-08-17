{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveFunctor, DeriveFoldable, DeriveTraversable, MultiWayIf #-}
module UntypedLambdaCalculus (Expr (Free, Var, Lam, App), ReaderAlg, eval, cataReader) where

import Control.Monad.Reader (Reader, runReader, local, reader, ask)
import Data.Bifunctor (bimap)
import Data.Functor ((<&>))
import Data.Functor.Foldable (Base, Recursive, cata)
import Data.Functor.Foldable.TH (makeBaseFunctor)

-- | A lambda calculus expression where variables are identified
-- | by their distance from their binding site (De Bruijn indices).
data Expr = Free String
          | Var Int
          | Lam String Expr
          | App Expr Expr

makeBaseFunctor ''Expr

type Algebra   f   a = f a ->          a
type ReaderAlg f s a = Algebra f (Reader s a)

cataReader :: Recursive r => ReaderAlg (Base r) s a -> s -> r -> a
cataReader f initialState x = runReader (cata f x) initialState

instance Show Expr where
  show = cataReader alg []
    where alg :: ReaderAlg ExprF [String] String
          alg (FreeF v) = return v
          alg (VarF  v) = reader (\vars -> vars !! v ++ ':' : show v)
          alg (LamF  v e) = do
            body <- local (v :) e
            return $ "(\\" ++ v ++ ". " ++ body ++ ")"
          alg (AppF f' x') = do
            f <- f'
            x <- x'
            return $ "(" ++ f ++ " " ++ x ++ ")"

-- | Determine whether the variable bound by a lambda expression is used in its body.
-- | This is used in eta reduction, where `(\x. f x)` reduces to `f` when `x` is not bound in `f`.
unbound :: Expr -> Bool
unbound = cataReader alg 0
  where alg :: ReaderAlg ExprF Int Bool
        alg (FreeF _)  = return True
        alg (VarF  v)  = reader (/= v)
        alg (AppF f x) = (&&) <$> f <*> x
        alg (LamF _ e) = local (+ 1) e

-- | When we bind a new variable, we enter a new scope.
-- | Since variables are identified by their distance from their binder,
-- | we must increment them to account for the incremented distance,
-- | thus embedding them into the new expression.
embed :: Expr -> Expr
embed (Var v)   = Var $ v + 1
embed (App f x) = App (embed f) (embed x)
embed x         = x

subst :: Expr -> Expr -> Expr
subst val = cataReader alg (0, val)
  where alg :: ReaderAlg ExprF (Int, Expr) Expr
        alg (FreeF x)  = return $ Free x
        alg (VarF x)   = ask <&> \(x', value) -> if
          | x == x'   -> value
          | x >  x'   -> Var $ x - 1
          | otherwise -> Var x
        alg (AppF f x) = App <$> f <*> x
        alg (LamF v e) = Lam v <$> local (bimap (+ 1) embed) e 

-- | Evaluate an expression to normal form.
eval :: Expr -> Expr
eval (App f' x) = case eval f' of
  -- Beta reduction.
  Lam _ e -> eval $ subst x e
  f -> App f (eval x)
eval o@(Lam _ (App f (Var 0)))
  -- Eta reduction. We know that `0` is not bound in `f`,
  -- so we can simply substitute it for undefined.
  | unbound f = eval $ subst undefined f
  | otherwise = o
eval o = o
