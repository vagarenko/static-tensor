{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE KindSignatures          #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TemplateHaskell         #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeInType              #-}
{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE NoImplicitPrelude       #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE ConstraintKinds         #-}

{-# OPTIONS_GHC -fno-solve-constant-dicts #-} -- See https://ghc.haskell.org/trac/ghc/ticket/13943#comment:2

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.List.Unrolled
-- Copyright   :  (C) 2017 Alexey Vagarenko
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Alexey Vagarenko (vagarenko@gmail.com)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module provides unrollable versions of functions on lists.
--
-- Classes in this module are assumed to be closed. You __should not__ create
-- new instances for them.
--
----------------------------------------------------------------------------

module Data.List.Unrolled (
      Append(..)
    , Drop(..)
    , Take(..)
    , splitAt
    , ChunksOf(..)
    , ChunksCount
    , Zip(..)
    , Zip3(..)
    , ZipWith(..)
    , Unzip(..)
    , Filter(..)
    , Map(..)
    , All(..)
    , Foldr(..)
    , Foldr1(..)
    , Foldl(..)
    , Foldl1(..)
    , foldMap
    , FoldMap
    , sum
    , Sum
    , Replicate(..)
    , EnumFromN(..)
    , EnumFromStepN(..)
) where

import Data.Type.Bool           (If)
import GHC.TypeLits             (Nat, type (+), type (-), type (<=?))

import Prelude  (Bool(..), otherwise, Num(..), error, Monoid(..), (.))

---------------------------------------------------------------------------------------------------
-- | Append two lists. Type param @l@ is the length of the left list.
class Append (n :: Nat) where
    append :: [a] -> [a] -> [a]

instance {-# OVERLAPPING #-} Append 0 where
    append _ ys = ys
    {-# INLINE append #-}

instance {-# OVERLAPPABLE #-} (Append (n - 1)) => Append n where
    append []       _  = error "append: Not enough elements in the list."
    append (x : xs) ys = x : append @(n - 1) xs ys
    {-# INLINE append #-}

---------------------------------------------------------------------------------------------------
-- | Drop @n@ elements from a list.
class Drop (n :: Nat) where
    drop :: [a] -> [a]

instance {-# OVERLAPPING #-} Drop 0 where
    drop xs = xs
    {-# INLINE drop #-}

instance {-# OVERLAPPABLE #-} (Drop (n - 1)) => Drop n where
    drop [] = error "drop: Not enough elements in the list."
    drop (_ : xs) = drop @(n - 1) xs
    {-# INLINE drop #-}

---------------------------------------------------------------------------------------------------
-- | Take @n@ elements from a list
class Take (n :: Nat) where
    take :: [a] -> [a]

instance {-# OVERLAPPING #-} Take 0 where
    take _ = []
    {-# INLINE take #-}

instance {-# OVERLAPPABLE #-} (Take (n - 1)) => Take n where
    take [] = error "take: Not enough elements in the list."
    take (x : xs) = x : take @(n - 1) xs
    {-# INLINE take #-}

---------------------------------------------------------------------------------------------------
-- | Split list at @n@-th element.
splitAt :: forall (n :: Nat) a. (Take n, Drop n) => [a] -> ([a], [a])
splitAt xs = (take @n xs, drop @n xs)

---------------------------------------------------------------------------------------------------
-- | Split list into chunks of the given length @c@. @n@ is length of the list.
class ChunksOf (n :: Nat) (c :: Nat) where
    chunksOf :: [a] -> [[a]]

instance {-# OVERLAPPING #-} ChunksOf 0 0 where
    chunksOf _ = []
    {-# INLINE chunksOf #-}

instance {-# OVERLAPPABLE #-} ChunksOf 0 c where
    chunksOf _ = []
    {-# INLINE chunksOf #-}

instance {-# OVERLAPPABLE #-} ChunksOf n 0 where
    chunksOf _ = []
    {-# INLINE chunksOf #-}

instance {-# OVERLAPPABLE #-} (Take c, Drop c, ChunksOf (n - 1) c) => ChunksOf n c where
    chunksOf xs =
        let (l, r) = splitAt @c xs
        in l : chunksOf @(n - 1) @c r
    {-# INLINE chunksOf #-}

-- | Number of resulting chunks when list of length @len@ split by chunks of length @clen@.
type family ChunksCount (len :: Nat) (clen :: Nat) where
    ChunksCount 0 _ = 0
    ChunksCount _ 0 = 0
    ChunksCount l c = If (l <=? c) 1 (1 + ChunksCount (l - c) c)

---------------------------------------------------------------------------------------------------
-- | Zip 2 lists together. Type param @n@ is the length of the first list.
class Zip (n :: Nat) where
    zip :: [a] -> [b] -> [(a, b)]

instance {-# OVERLAPPING #-} Zip 0 where
    zip _ _ = []
    {-# INLINE zip #-}

instance {-# OVERLAPPABLE #-} (Zip (n - 1)) => Zip n where
    zip (x : xs) (y : ys) = (x, y) : zip @(n - 1) xs ys
    zip (_ : _ ) []       = []
    zip []        _       = error "zip: Not enough elements in the first list."
    {-# INLINE zip #-}

---------------------------------------------------------------------------------------------------
-- | Zip 3 lists together. Type param @n@ is the length of the first list.
class Zip3 (n :: Nat) where
    zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]

instance {-# OVERLAPPING #-} Zip3 0 where
    zip3 _ _ _ = []
    {-# INLINE zip3 #-}

instance {-# OVERLAPPABLE #-} (Zip3 (n - 1)) => Zip3 n where
    zip3 (x : xs) (y : ys) (z : zs) = (x, y, z) : zip3 @(n - 1) xs ys zs
    zip3 (_ : _ ) []       _        = []
    zip3 (_ : _ ) _        []       = []
    zip3 []       _        _        = error "zip3: Not enough elements in the first list."
    {-# INLINE zip3 #-}

---------------------------------------------------------------------------------------------------
-- | Unzip a list. Type param @n@ is the length of the list.
class Unzip (n :: Nat) where
    unzip :: [(a, b)] -> ([a], [b])

instance {-# OVERLAPPING #-} Unzip 0 where
    unzip _ = ([], [])
    {-# INLINE unzip #-}

instance {-# OVERLAPPABLE #-} (Unzip (n - 1)) => Unzip n where
    unzip []       = error "unzip: Not enough elements in the list."
    unzip (x : xs) = (\(a, b) (as, bs) -> (a : as, b : bs)) x (unzip @(n - 1) xs)
    {-# INLINE unzip #-}

---------------------------------------------------------------------------------------------------
-- | Zip 2 lists together using given function. Type param @n@ is the length of the first list.
class ZipWith (n :: Nat) where
    zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]

instance {-# OVERLAPPING #-} ZipWith 0 where
    zipWith _ _ _ = []
    {-# INLINE zipWith #-}

instance {-# OVERLAPPABLE #-} (ZipWith (n - 1)) => ZipWith n where
    zipWith f (x : xs) (y : ys) = f x y : zipWith @(n - 1) f xs ys
    zipWith _ (_ : _ ) []       = []
    zipWith _ []       _        = error "zipWith: Not enough elements in the first list."
    {-# INLINE zipWith #-}

---------------------------------------------------------------------------------------------------
-- | Filter list with given predicate. Type param @n@ is the length of the list.
class Filter (n :: Nat) where
    filter :: (a -> Bool) -> [a] -> [a]

instance {-# OVERLAPPING #-} Filter 0 where
    filter _ _ = []
    {-# INLINE filter #-}

instance {-# OVERLAPPABLE #-} (Filter (n - 1)) => Filter n where
    filter _ []       = error "filter: Not enough elements in the list."
    filter f (x : xs)
        | f x       = x : filter @(n - 1) f xs
        | otherwise = filter @(n - 1) f xs
    {-# INLINE filter #-}

---------------------------------------------------------------------------------------------------
-- | Apply function to all elements of a list. Type param @n@ is the length of the list.
class Map (n :: Nat) where
    map :: (a -> b) -> [a] -> [b]

instance {-# OVERLAPPING #-} Map 0 where
    map _ _ = []
    {-# INLINE map #-}

instance {-# OVERLAPPABLE #-} (Map (n - 1)) => Map n where
    map _ []       = error "map: Not enough elements in the list."
    map f (x : xs) = f x : map @(n - 1) f xs
    {-# INLINE map #-}

---------------------------------------------------------------------------------------------------
-- | Check if all elements of the list satisfy the predicate. Type param @n@ is the length of the list.
class All (n :: Nat) where
    all :: (a -> Bool) -> [a] -> Bool

instance {-# OVERLAPPING #-} All 0 where
    all _ _ = True
    {-# INLINE all #-}

instance {-# OVERLAPPABLE #-} (All (n - 1)) => All n where
    all _ []        = error "all: Not enough elements in the list."
    all f (x : xs)
        | f x       = all @(n - 1) f xs
        | otherwise = False
    {-# INLINE all #-}

---------------------------------------------------------------------------------------------------
-- | Right fold of a list of length @n@.
class Foldr (n :: Nat) where
    foldr :: (a -> b -> b) -> b -> [a] -> b

instance {-# OVERLAPPING #-} Foldr 0 where
    foldr _ z _ = z
    {-# INLINE foldr #-}

instance {-# OVERLAPPABLE #-} (Foldr (n - 1)) => Foldr n where
    foldr _ _ []       = error "foldr: Not enough elements in the list."
    foldr f z (x : xs) = f x (foldr @(n - 1) f z xs)
    {-# INLINE foldr #-}

---------------------------------------------------------------------------------------------------
-- | Right fold of a list of length @n@ with no base element.
class Foldr1 (n :: Nat) where
    foldr1 :: (a -> a -> a) -> [a] -> a

instance {-# OVERLAPPING #-} Foldr1 1 where
    foldr1 _ []      = error "foldr1: Not enough elements in the list."
    foldr1 _ (x : _) = x
    {-# INLINE foldr1 #-}

instance {-# OVERLAPPABLE #-} (Foldr1 (n - 1)) => Foldr1 n where
    foldr1 _ []       = error "foldr1: Empty list."
    foldr1 f (x : xs) = f x (foldr1 @(n - 1) f xs)
    {-# INLINE foldr1 #-}

---------------------------------------------------------------------------------------------------
-- | Left fold of a list of length @n@.
class Foldl (n :: Nat) where
    foldl :: (b -> a -> b) -> b -> [a] -> b

instance {-# OVERLAPPING #-} Foldl 0 where
    foldl _ z _ = z
    {-# INLINE foldl #-}

instance {-# OVERLAPPABLE #-} (Foldl (n - 1)) => Foldl n where
    foldl _ _ []       = error "foldl: Not enough elements in the list."
    foldl f z (x : xs) = f (foldl @(n - 1) f z xs) x
    {-# INLINE foldl #-}

---------------------------------------------------------------------------------------------------
-- | Right fold of a list of length @n@ with no base element.
class Foldl1 (n :: Nat) where
    foldl1 :: (a -> a -> a) -> [a] -> a

instance {-# OVERLAPPING #-} Foldl1 1 where
    foldl1 _ []      = error "foldl1: Not enough elements in the list."
    foldl1 _ (x : _) = x
    {-# INLINE foldl1 #-}

instance {-# OVERLAPPABLE #-} (Foldl1 (n - 1)) => Foldl1 n where
    foldl1 _ []       = error "foldl1: Empty list."
    foldl1 f (x : xs) = f (foldl1 @(n - 1) f xs) x
    {-# INLINE foldl1 #-}

---------------------------------------------------------------------------------------------------
-- | Map each element of the list of length @n@ to a monoid, and combine the results.
foldMap :: forall (n :: Nat) m a.
           (FoldMap n m) =>
           (a -> m) -> [a] -> m
foldMap f = foldr @n (mappend . f) mempty
{-# INLINE foldMap #-}

-- | Constraint of the 'foldMap' function.
type FoldMap (n :: Nat) m = (Monoid m, Foldr n)

---------------------------------------------------------------------------------------------------
-- | Sum of the elements of the list of length @n@.
sum :: forall (n :: Nat) a.
       (Sum n a) =>
       [a] -> a
sum = foldr @n (+) 0
{-# INLINE sum #-}

-- | Constraint of the 'sum' function.
type Sum (n :: Nat) a = (Foldr n, Num a)

---------------------------------------------------------------------------------------------------
-- | Fill the list of length @n@ with the same values.
class Replicate (n :: Nat) where
    replicate :: a -> [a]

instance {-# OVERLAPPING #-} Replicate 0 where
    replicate _ = []
    {-# INLINE replicate #-}

instance {-# OVERLAPPABLE #-} (Replicate (n - 1)) => Replicate n where
    replicate a = a : replicate @(n - 1) a
    {-# INLINE replicate #-}

---------------------------------------------------------------------------------------------------
-- | Enumeration of length @n@ starting from given value.
class EnumFromN (n :: Nat) where
    enumFromN :: (Num a)
        => a        -- ^ Starting value.
        -> [a]

instance {-# OVERLAPPING #-} EnumFromN 0 where
    enumFromN _ = []
    {-# INLINE enumFromN #-}

instance {-# OVERLAPPABLE #-} (EnumFromN (n - 1)) => EnumFromN n where
    enumFromN a = a : enumFromN @(n - 1) (a + 1)
    {-# INLINE enumFromN #-}

---------------------------------------------------------------------------------------------------
-- | Enumeration of length @n@ starting from given value with given step.
class EnumFromStepN (n :: Nat) where
    enumFromStepN :: (Num a)
        => a        -- ^ Starting value.
        -> a        -- ^ Step.
        -> [a]

instance {-# OVERLAPPING #-} EnumFromStepN 0 where
    enumFromStepN _ _ = []
    {-# INLINE enumFromStepN #-}

instance {-# OVERLAPPABLE #-} (EnumFromStepN (n - 1)) => EnumFromStepN n where
    enumFromStepN a s = a : enumFromStepN @(n - 1) (a + s) s
    {-# INLINE enumFromStepN #-}
