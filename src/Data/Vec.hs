{-# LANGUAGE GADTs, TypeOperators, TypeSynonymInstances, FlexibleInstances #-}
module Data.Vec (Vec (Empty, (:.)), (!.), elemIndexVec) where

import Data.Fin (Fin (Zero, Succ))
import Data.Type.Nat (Nat, Zero, Succ)

data Vec n a where
  Empty :: Vec Zero a
  (:.)  :: Nat n => a -> Vec n a -> Vec (Succ n) a

instance Nat n => Functor (Vec n) where
  fmap _ Empty     = Empty
  fmap f (x :. xs) = f x :. fmap f xs

instance Nat n => Foldable (Vec n) where
  foldr _ base Empty     = base
  foldr f base (x :. xs) = x `f` foldr f base xs

(!.) :: Nat n => Vec n a -> Fin n -> a
(x :. _ ) !. Zero     = x
(_ :. xs) !. (Succ n) = xs !. n
_         !. _        = error "Impossible"

elemIndexVec :: (Eq a, Nat n) => a -> Vec n a -> Maybe (Fin n)
elemIndexVec _ Empty = Nothing
elemIndexVec x (x' :. xs)
  | x == x'   = Just Zero
  | otherwise = Succ <$> elemIndexVec x xs
