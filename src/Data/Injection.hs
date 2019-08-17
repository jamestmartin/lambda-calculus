{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Data.Injection (Injection, inject) where

class Injection a b where
  inject :: a -> b

instance Injection a a where
  inject = id

instance Injection a (Maybe a) where
  inject = Just
