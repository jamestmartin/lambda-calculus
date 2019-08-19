{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveFunctor, DeriveFoldable, DeriveTraversable, MultiWayIf #-}
module UntypedLambdaCalculus (Expr (Free, Var, Lam, App, Nil), ReaderAlg, eval, cataReader) where

import Control.Monad.Reader (Reader, runReader, local, reader, ask)
import Data.Foldable (fold)
import Data.Functor ((<&>))
import Data.Functor.Foldable (Base, Recursive, cata, embed, project)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Monoid (Any (Any, getAny))

-- | A lambda calculus expression where variables are identified
-- | by their distance from their binding site (De Bruijn indices).
data Expr = Free String
          | Var Int
          | Lam String Expr
          | App Expr Expr
          | Nil

makeBaseFunctor ''Expr

type Algebra   f   a = f a ->          a
type ReaderAlg f s a = Algebra f (Reader s a)

cataReader :: Recursive r => ReaderAlg (Base r) s a -> s -> r -> a
cataReader f initialState x = runReader (cata f x) initialState

instance Show Expr where
  show = cataReader alg []
    where alg :: ReaderAlg ExprF [String] String
          alg (FreeF v)    = return v
          alg (VarF  i)    = reader (\vars -> vars !! i ++ ':' : show i)
          alg (LamF v e) = do
            body <- local (v :) e
            return $ "(\\" ++ v ++ ". " ++ body ++ ")"
          alg (AppF f' x') = do
            f <- f'
            x <- x'
            return $ "(" ++ f ++ " " ++ x ++ ")"
          alg NilF = return "()"

-- | Is the innermost bound variable of this subexpression (`Var 0`) used in its body?
-- | For example: in `\x. a:1 x:0 b:2`, `x:0` is bound in `a:1 x:0 b:2`.
-- | On the other hand, in `\x. a:3 b:2 c:1`, it is not.
bound :: Expr -> Bool
bound = getAny . cataReader alg 0
  where alg :: ReaderAlg ExprF Int Any
        alg (VarF index) = reader (Any . (== index))
        alg (LamF _ e)   = local (+ 1) e
        alg x            = fold <$> sequenceA x

-- | Opposite of `bound`.
unbound :: Expr -> Bool
unbound = not . bound

-- | When we bind a new variable, we enter a new scope.
-- | Since variables are identified by their distance from their binder,
-- | we must increment them to account for the incremented distance,
-- | thus embedding them into the new expression.
liftExpr :: Int -> Expr -> Expr
liftExpr n (Var i)     = Var $ i + n
liftExpr _ o@(Lam _ _) = o
liftExpr n x           = embed $ fmap (liftExpr n) $ project x

subst :: Expr -> Expr -> Expr
subst val = cataReader alg 0
  where alg :: ReaderAlg ExprF Int Expr
        alg (VarF i)   = ask <&> \bindingDepth -> if
          | i == bindingDepth -> liftExpr bindingDepth val
          | i >  bindingDepth -> Var $ i - 1
          | otherwise -> Var i
        alg (LamF v e) = Lam v <$> local (+ 1) e
        alg x = embed <$> sequence x

-- | Generalized eta reduction. I (almost certainly re-)invented it myself.
etaReduce :: Expr -> Expr
-- Degenerate case
-- The identity function reduces to the syntactic identity, `Nil`.
etaReduce (Lam _ (Var 0)) = Nil
-- `\x. f x -> f` if `x` is not bound in `f`.
etaReduce o@(Lam _ (App f (Var 0)))
  | unbound f = eval $ subst undefined f
  | otherwise = o
-- `\x y. f y -> \x. f` if `y` is not bound in `f`;
-- the resultant term may itself be eta-reducible.
etaReduce (Lam v e'@(Lam _ _)) = case etaReduce e' of
  e@(Lam _ _) -> Lam v e
  e -> etaReduce $ Lam v e
etaReduce x = x

betaReduce :: Expr -> Expr
betaReduce (App f' x) = case eval f' of
  Lam _ e -> eval $ subst x e
  Nil -> eval x
  f -> App f $ eval x
betaReduce x = x

-- | Evaluate an expression to normal form.
eval :: Expr -> Expr
eval a@(App _ _) = betaReduce a
eval l@(Lam _ _) = etaReduce l
eval o = o
