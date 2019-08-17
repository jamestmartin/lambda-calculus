{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
module Data.Type.Nat (Nat, Zero, Succ, TOrdering, GT, EQ, LT, Compare, GTorEQ) where

class Nat n
data Zero
instance Nat Zero
data Succ n
instance (Nat n) => Nat (Succ n)

class TOrdering c
data GT
instance TOrdering GT
data EQ
instance TOrdering EQ
data LT

class Compare n m c | n m -> c

instance Compare Zero Zero EQ
instance Nat n => Compare (Succ n) Zero GT
instance Nat n => Compare Zero (Succ n) LT
instance (Nat n, Nat m, TOrdering c, Compare n m c) => Compare (Succ n) (Succ m) c

class GTorEQ n m
instance GTorEQ Zero Zero
instance Nat n => GTorEQ n n
instance Nat n => GTorEQ (Succ n) Zero
instance (Nat n, Nat m, GTorEQ n m) => GTorEQ (Succ n) (Succ m)
