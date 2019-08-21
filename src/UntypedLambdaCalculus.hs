{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveFunctor, DeriveFoldable, DeriveTraversable, MultiWayIf #-}
module UntypedLambdaCalculus (Expr (Free, Var, Lam, App), ReaderAlg, eval, cataReader) where

import Control.Monad.Reader (Reader, runReader, local, reader)
import Data.Foldable (fold)
import Data.Functor.Foldable (Base, Recursive, cata, embed, project)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Monoid (Any (Any, getAny))

-- | A lambda calculus expression where variables are identified
-- | by their distance from their binding site (De Bruijn indices).
data Expr = Free String
          | Var Int
          | Lam String Expr
          | App Expr Expr
          | Subst Int Expr Expr

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
          alg (SubstF index val' body') = do
            body <- local ("SUBSTVAR" :) body'
            val <- val'
            return $ body ++ "[ " ++ show index ++ " := " ++ val ++ " ]"

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

substitute :: Int -> Expr -> Expr -> Expr
substitute index val v@(Var index')
  | index == index' = val
  | index <  index' = Var $ index' - 1
  | otherwise       = v
substitute index val (Lam name body) = Lam name $ Subst (index + 1) (liftExpr 1 val) body
substitute index val (Subst index2 val2 body) =
  substitute index val $ substitute index2 val2 body
substitute index val x = embed $ fmap (Subst index val) $ project x

etaReduce :: String -> Expr -> Expr
-- `\x. f x -> f` if `x` is not bound in `f`.
etaReduce name o@(App f (Var 0))
  | unbound f = eval $ Subst 0 undefined f
  | otherwise = Lam name $ o
-- `\x y. f y -> \x. f` if `y` is not bound in `f`;
-- the resultant term may itself be eta-reducible.
etaReduce name (Lam name' body') = case etaReduce name' body' of
  body@(Lam _ _) -> Lam name body
  body -> etaReduce name body
etaReduce name body = Lam name body

betaReduce :: Expr -> Expr -> Expr
betaReduce f' x = case eval f' of
  Lam _ e -> eval $ Subst 0 x e
  f -> App f $ eval x

-- | Evaluate an expression to normal form.
eval :: Expr -> Expr
eval (Subst index val body) = eval $ substitute index val body
eval (App f x)              = betaReduce f x
eval (Lam name body)        = etaReduce name body
eval o = o
