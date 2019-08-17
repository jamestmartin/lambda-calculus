{-# LANGUAGE GADTs, TypeOperators, TypeSynonymInstances, FlexibleInstances #-}
module Data.Vec (Vec (Empty, (:.)), (!.), elemIndexVec, vmap) where

import Data.Fin (Fin (Zero, Succ))
import Data.Type.Nat (Nat, Zero, Succ)

data Vec a n where
  Empty :: Vec a Zero
  (:.)  :: Nat n => a -> Vec a n -> Vec a (Succ n)

-- | Equivalent to fmap. It is impossible to implement Functor on Vec for stupid reasons.
vmap :: (a -> b) -> Vec a n -> Vec b n
vmap _ Empty     = Empty
vmap f (x :. xs) = f x :. vmap f xs

(!.) :: Nat n => Vec a n -> Fin n -> a
(!.) (x :. _ ) Zero     = x
(!.) (_ :. xs) (Succ n) = xs !. n

elemIndexVec :: (Eq a, Nat n) => a -> Vec a n -> Maybe (Fin n)
elemIndexVec _ Empty = Nothing
elemIndexVec x (x' :. xs)
  | x == x'   = Just Zero
  | otherwise = Succ <$> elemIndexVec x xs
