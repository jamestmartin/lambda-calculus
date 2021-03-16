module Data.Stream (Stream (Cons), filter, fromList) where

import Data.Functor.Foldable (Base, Corecursive, Recursive, embed, project, ana)
import Prelude hiding (filter, head, tail)

data Stream a = Cons a (Stream a)

type instance Base (Stream a) = (,) a

instance Recursive (Stream a) where
  project (Cons x xs) = (x, xs)

instance Corecursive (Stream a) where
  embed (x, xs) = Cons x xs

filter :: (a -> Bool) -> Stream a -> Stream a
filter p = ana \case
  Cons x xs
    | p x -> (x, xs)
    | otherwise -> project xs

fromList :: [a] -> Stream a
fromList = ana coalg
  where
    coalg (x : xs) = (x, xs)
    coalg [] = error "Attempted to turn finite list into stream"
