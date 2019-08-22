{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveFunctor, DeriveFoldable, DeriveTraversable, MultiWayIf, LambdaCase, BlockArguments #-}
module UntypedLambdaCalculus (Expr (Free, Var, Lam, App), ReaderAlg, eval, cataReader) where

import Control.Monad.Reader (Reader, runReader, local, reader, ask, asks)
import Control.Monad.Writer (Writer, runWriter, listen, tell)
import Data.Foldable (fold)
import Data.Functor ((<&>))
import Data.Functor.Foldable (Base, Recursive, cata, embed, project)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Monoid (Any (Any, getAny))

-- | An expression using the Reverse De Bruijn representation.
-- | Like De Bruijn representation, variables are named according to their binding site.
-- | However, instead of being named by the number of binders between variable and its binder,
-- | here variables are represented by the distance between their binder and the top level.
data Expr = Free String
          -- Var index is bound `index` in from the outermost binder.
          -- The outermost binder's name is the last element of the list.
          | Var Int
          -- This lambda is `index` bindings away from the outermost binding.
          -- If the index is `0`, then this is the outermost binder.
          | Lam String Int Expr
          | App Expr Expr

makeBaseFunctor ''Expr

type Algebra   f   a = f a ->          a
type ReaderAlg f s a = Algebra f (Reader s a)

cataReader :: Recursive r => ReaderAlg (Base r) s a -> s -> r -> a
cataReader f initialState x = runReader (cata f x) initialState

indexOr :: a -> Int -> [a] -> a
indexOr def index xs
  | index < length xs && index >= 0 = xs !! index
  | otherwise = def

instance Show Expr where
  show = flip cataReader [] \case
    FreeF name -> return name
    VarF index -> asks (\boundNames -> indexOr "" (length boundNames - index - 1) boundNames)
      <&> \name -> name ++ ":" ++ show index
    LamF name index body' -> local (name :) body' <&> \body ->
      "(\\" ++ name ++ ":" ++ show index ++ ". " ++ body ++ ")"
    AppF f' x' -> f' >>= \f -> x' <&> \x ->
      "(" ++ f ++ " " ++ x ++ ")"
    
eval :: Expr -> Expr
eval x = case reduce innerReduced of
    Just expr -> eval expr
    Nothing -> x
  where innerReduced = embed $ fmap eval $ project x

reduce :: Expr -> Maybe Expr
reduce (Lam name index body) = etaReduce name index body
reduce (App f x)             = betaReduce f x
reduce _                     = Nothing

betaReduce :: Expr -> Expr -> Maybe Expr
betaReduce (Lam name index body) x = Just $ subst index x body
betaReduce _                     _ = Nothing

etaReduce :: String -> Int -> Expr -> Maybe Expr
etaReduce name index body@(App f (Var index'))
  -- If the variable bound by this lambda is only used in the right hand of the outermost app,
  -- then we may delete this function. The absolute position of all binding terms inside
  -- this one has been decreased by the removal of this lambda, and must be renumbered.
  | index == index' && unbound index f = Just $ subst index undefined f
  | otherwise = Nothing
etaReduce _ _ _ = Nothing

unbound :: Int -> Expr -> Bool
unbound index = not . bound index

bound :: Int -> Expr -> Bool
bound index = getAny . cata \case
  VarF index' -> Any $ index == index'
  expr -> fold expr

embedExpr :: Int -> Expr -> Expr
embedExpr index (Free name) = Free name
embedExpr index (Var index')
  | index' >= index = Var $ index' + 1
  | otherwise = Var index'
embedExpr index (App f x) = App (embedExpr index f) (embedExpr index x)
embedExpr index (Lam name index' body) = Lam name (index' + 1) $ embedExpr index body

subst :: Int -> Expr -> Expr -> Expr
subst index val (Free name) = Free name
subst index val (Var index')
  | index == index' = val
  -- There is now one fewer binding site between the innermost binding site and `index`,
  -- thus if the binding site is further in than ours, it must be decremented.
  | index <  index' = Var $ index' - 1
  | otherwise = Var index'
subst index val (App f x) = App (subst index val f) (subst index val x)
subst index val (Lam name index' body) = Lam name (index' - 1) $ subst index (embedExpr index val) body
