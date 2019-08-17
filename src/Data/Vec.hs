{-# LANGUAGE GADTs, TypeOperators, DataKinds #-}
module Data.Vec (Vec (Empty, (:.)), (!.), elemIndex) where

import Data.Fin (Fin (FZ, FS))
import Data.Nat (Nat (Z, S))

data Vec n a where
  Empty :: Vec 'Z a
  (:.)  :: a -> Vec n a -> Vec ('S n) a

(!.) :: Vec n a -> Fin n -> a
(x :. _ ) !. FZ     = x
(_ :. xs) !. (FS n) = xs !. n
_         !. _      = error "Impossible"

elemIndex :: Eq a => a -> Vec n a -> Maybe (Fin n)
elemIndex _ Empty = Nothing
elemIndex x (x' :. xs)
  | x == x'   = Just FZ
  | otherwise = FS <$> elemIndex x xs
