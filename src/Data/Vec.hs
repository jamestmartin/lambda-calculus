module Data.Vec where

import Data.Type.Nat

-- | See the documentation of 'Data.Type.Nat' to see why I don't just depend on the `vec` package.
data Vec :: Nat -> * -> * where
  VNil  :: Vec 'Z a
  (:::) :: a -> Vec n a -> Vec ('S n) a
