{-# LANGUAGE GADTs, DataKinds, KindSignatures #-}
module Data.Fin (Fin (FZ, FS), toInt, coerceFin, pred, extract) where

import Data.Nat (Nat (S))
import Prelude hiding (pred)
import Unsafe.Coerce (unsafeCoerce)

-- | A type with `n` inhabitants, or alternatively,
-- | a natural number less than the upper bound parameter.
data Fin :: Nat -> * where
  -- Fin Zero is uninhabited. Fin (Succ Zero) has one inhabitant.
  FZ :: Fin ('S n)
  FS :: Fin n -> Fin ('S n)

instance Eq (Fin n) where
  FZ   == FZ   = True
  FS n == FS m = n == m
  _    == _    = False

instance Ord (Fin n) where
  FZ   `compare` FZ   = EQ
  FS _ `compare` FZ   = GT
  FZ   `compare` FS _ = LT
  FS n `compare` FS m = n `compare` m

toInt :: Fin n -> Int
toInt FZ     = 0
toInt (FS n) = 1 + toInt n

instance Show (Fin n) where
  show = show . toInt

coerceFin :: Fin n -> Fin ('S n)
coerceFin FZ     = FZ
coerceFin (FS n) = FS $ coerceFin n

pred :: Fin ('S n) -> Maybe (Fin n)
pred FZ     = Nothing
pred (FS n) = Just n

-- | Match against an element in `Fin`, removing it from its domain.
extract :: Fin ('S n) -> Fin ('S n) -> Maybe (Fin n)
extract n m
  | n == m = Nothing
  | n >  m = pred n
  -- I am convinced it is not possible to prove to the compiler
  -- that this function is valid without `unsafeCoerce`.
  | otherwise = Just $ unsafeCoerce n
