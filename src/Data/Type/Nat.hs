-- | I would rather depend on the `fin` package than export my own Nat type,
-- but I couldn't figure out how to write substitution with their definition of SNat,
-- because it uses `SNatI n =>` instead of `SNat n ->` in the recursive case,
-- and `withSNat` and `snat` were both providing ambigous type variables and all that.
-- If anyone could fix it for me, I would gladly accept a PR.
module Data.Type.Nat where

import Data.Type.Equality ((:~:)(Refl))
import Numeric.Natural (Natural)

data Nat = Z | S Nat

instance Show Nat where
  show = show . natToNatural

data SNat :: Nat -> * where
  SZ :: SNat 'Z
  SS :: SNat n -> SNat ('S n)

instance Show (SNat n) where
  show = show . snatToNatural

type family Plus (n :: Nat) (m :: Nat) = (sum :: Nat) where
  Plus Z      m = m
  Plus ('S n) m = 'S (Plus n m)

class               SNatI (n :: Nat) where snat :: SNat n
instance            SNatI 'Z         where snat = SZ
instance SNatI n => SNatI ('S n)     where snat = SS snat

natToNatural :: Nat -> Natural
natToNatural Z     = 0
natToNatural (S n) = 1 + natToNatural n

snatToNat :: forall n. SNat n -> Nat
snatToNat SZ     = Z
snatToNat (SS n) = S $ snatToNat n

snatToNatural :: forall n. SNat n -> Natural
snatToNatural = natToNatural . snatToNat

plusZero :: SNat n -> Plus n 'Z :~: n
-- trivial: Z + n = n by definition of equality,
-- and in this case n = Z and thus Z + Z = Z.
plusZero SZ     = Refl
-- if n + Z = n, then S n + Z = S n.
plusZero (SS n) = case plusZero n of Refl -> Refl

plusSuc :: SNat n -> SNat m -> Plus n ('S m) :~: 'S (Plus n m)
-- trivial: Z + n = n by definition of equality,
-- and in this case n = S m, and thus Z + S m = S m.
plusSuc SZ     _ = Refl
-- if n + S m = S (n + m), then S n + S m = S (S n + m).
plusSuc (SS n) m = case plusSuc n m of Refl -> Refl
