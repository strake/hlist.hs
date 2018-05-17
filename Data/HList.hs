module Data.HList where

import Prelude hiding (curry, uncurry)

infixr 5 :.
data List as where
    Nil :: List '[]
    (:.) :: a -> List as -> List (a ': as)

instance Eq (List '[]) where
    Nil == Nil = True

instance (Eq a, Eq (List as)) => Eq (List (a ': as)) where
    x:.xs == y:.ys = x == y && xs == ys

instance Ord (List '[]) where
    Nil `compare` Nil = EQ

instance (Ord a, Ord (List as)) => Ord (List (a ': as)) where
    (x:.xs) `compare` (y:.ys) = x `compare` y <> xs `compare` ys

type family Foldr f z xs where
    Foldr f z '[] = z
    Foldr f z (x ': xs) = f x (Foldr f z xs)

class Curry as where
    uncurry :: Foldr (->) b as -> List as -> b
    curry :: (List as -> b) -> Foldr (->) b as

instance Curry '[] where
    uncurry f _ = f
    curry f = f Nil

instance Curry as => Curry (a ': as) where
    uncurry f (x:.xs) = uncurry (f x) xs
    curry f x = curry $ f . (x:.)
