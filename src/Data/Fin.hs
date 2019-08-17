{-# LANGUAGE TypeFamilies, GADTs, MultiParamTypeClasses, FlexibleInstances #-}
module Data.Fin (Fin (Zero, Succ), toInt, pred, finRemove) where

import Data.Functor.Foldable (Base, Recursive, project, cata)
import Data.Injection (Injection, inject)
import Data.Type.Nat (Nat, Zero, Succ, GTorEQ)
import Prelude hiding (pred)
import Unsafe.Coerce (unsafeCoerce)

data Fin n where
  -- Fin Zero is uninhabited. Fin (Succ Zero) has one inhabitant.
  Zero :: Nat n => Fin (Succ n)
  Succ :: Nat n => Fin n -> Fin (Succ n)

type instance Base (Fin n) = Maybe

instance (Nat n) => Injection (Fin n) (Fin (Succ n)) where
  inject Zero = Zero
  inject (Succ n) = Succ (inject n)

instance (Nat n) => Recursive (Fin n) where
  project Zero     = Nothing
  project (Succ n) = Just $ inject n

instance (Nat n) => Eq (Fin n) where
  Zero == Zero = True
  Succ n == Succ m = n == m
  _ == _ = False

instance Nat n => Ord (Fin n) where
  compare Zero     Zero     = EQ
  compare (Succ n) Zero     = GT
  compare Zero     (Succ n) = LT
  compare (Succ n) (Succ m) = compare n m

toInt :: Nat n => Fin n -> Int
toInt = cata alg
  where alg Nothing  = 0
        alg (Just n) = n + 1

instance (Nat n) => Show (Fin n) where
  show = show . toInt

pred :: Nat n => Fin (Succ n) -> Maybe (Fin n)
pred Zero     = Nothing
pred (Succ n) = Just n

-- | Remove an element from a `Fin`'s domain.
-- | Like a generalized `pred`, only you can remove elements other than `Zero`.
finRemove :: Nat n => Fin (Succ n) -> Fin (Succ n) -> Maybe (Fin n)
finRemove n m
  | n == m = Nothing
  | n >  m = pred n
  -- I am convinced it is not possible to prove to the compiler
  -- that this function is valid without `unsafeCoerce`.
  | n <  m = Just $ unsafeCoerce n
