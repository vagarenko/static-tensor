{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}           
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}           
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE StandaloneDeriving    #-}           
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Vector.Static
-- Copyright   :  (C) 2017 Alexey Vagarenko
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Alexey Vagarenko (vagarenko@gmail.com)
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------

module Data.Vector.Static (
    -- * Vector types
      Vector
    , VectorConstructor
    , IsVector
    -- ** Construction
    , vector
    -- ** Vector Operations
    , normalize
    , NormalizedVector
    , Normalize
    , unNormalizedVector
    , norm
    , vectorLenSquare
    , VectorLenSquare
    , vectorLen
    , VectorLen
    , dot
    , Dot
    , cross
    , toHomogenous
    , fromHomogenous
    -- ** Generating vector instances
    , genVectorInstance
) where
  
import Data.Containers       (MonoZip(..))
import Data.MonoTraversable  (omap, osum)
import Data.Tensor.Static    ( IsTensor(..), scale, Scale, TensorConstructor, withTensor
                             , MonoFunctorCtx, MonoFoldableCtx, MonoZipCtx)
import Data.Tensor.Static.TH (genTensorInstance)
import GHC.Generics          (Generic)
import GHC.TypeLits          (Nat)
import Language.Haskell.TH   (Q, Name, Dec)
import qualified Data.List.NonEmpty as N

---------------------------------------------------------------------------------------------------
-- | N-dimensional vector.
type Vector n e = Tensor '[n] e

-- | Type of vector data constructor.
type VectorConstructor n e = TensorConstructor '[n] e

-- | Vector constraint.
type IsVector n e = IsTensor '[n] e

-- | Normalized vector.
newtype NormalizedVector n e =
    NormalizedVector
        { unNormalizedVector :: Vector n e
            -- ^ unwrap 'NormalizedVector'. Note: this does not give you original vector back.
            --
            -- @unNormalizedVector . normalize /= id@
            --
        }
    deriving (Generic)
    
deriving instance (Eq   (Vector n e)) => Eq   (NormalizedVector n e)
deriving instance (Show (Vector n e)) => Show (NormalizedVector n e)

---------------------------------------------------------------------------------------------------
-- | Alias for a conrete vector data constructor.
vector :: forall n e. (IsVector n e) => VectorConstructor n e
vector = tensor @'[n] @e
{-# INLINE vector #-}

-- | Get square of length of a vector.
vectorLenSquare :: (VectorLenSquare n e) => Vector n e -> e
vectorLenSquare = osum . omap (\x -> x * x)
{-# INLINE vectorLenSquare #-}

-- | Constraints for 'vectorLenSquare' function.
type VectorLenSquare (n :: Nat) e =
    ( Num e
    , IsVector n e
    , MonoFunctorCtx '[n] e
    , MonoFoldableCtx '[n] e
    )

-- | Get length of a vector.    
vectorLen :: (VectorLen n e) => Vector n e -> e
vectorLen = sqrt . vectorLenSquare
{-# INLINE vectorLen #-}

-- | Constraints for 'vectorLen' function.
type VectorLen (n :: Nat) e =
    ( Floating e
    , VectorLenSquare n e
    )

-- | Normalize vector.
normalize :: (Normalize n e) => Vector n e -> NormalizedVector n e
normalize v = NormalizedVector $ scale v (1 / vectorLen v)
{-# INLINE normalize #-}

-- | Constraints for 'normalize' function.
type Normalize (n :: Nat) e =
    ( VectorLen n e
    , Scale '[n] e
    )

-- | Normalize vector but don't wrap it in 'NormalizedVector'.
norm :: (Normalize n e) => Vector n e -> Vector n e
norm = unNormalizedVector . normalize
{-# INLINE norm #-}

-- | Dot product of two vectors.
dot :: (Dot n e) => Vector n e -> Vector n e -> e
dot v1 v2 = osum $ ozipWith (*) v1 v2
{-# INLINE dot #-}

-- | Constraints for 'dot' function.
type Dot (n :: Nat) e =
    ( Num e
    , IsVector n e
    , MonoFunctorCtx '[n] e
    , MonoFoldableCtx '[n] e
    , MonoZipCtx '[n] e
    )

---------------------------------------------------------------------------------------------------
-- | Cross product is only defined for 3-dimensional vectors.
cross :: (Num e, IsVector 3 e) => Vector 3 e -> Vector 3 e -> Vector 3 e
cross v0 v1 =
    withTensor v0 $ \x0 y0 z0 ->
        withTensor v1 $ \x1 y1 z1 ->
            vector @3 (y0 * z1 - z0 * y1) (z0 * x1 - x0 * z1) (x0 * y1 - y0 * x1)
{-# INLINE cross #-}

---------------------------------------------------------------------------------------------------
-- | Convert 3-dimensional vector to 4-dimensional vector by setting the last element to @1@.
toHomogenous :: (Num e, IsVector 3 e, IsVector 4 e) => Vector 3 e -> Vector 4 e
toHomogenous v = withTensor v $ \x y z -> vector @4 x y z 1
{-# INLINE toHomogenous #-}

-- | Convert 4-dimensional vector to 3-dimensional vector by dividing first 3 coords by the last.
--   The last element must not be zero!
fromHomogenous :: (Fractional e, IsVector 3 e, IsVector 4 e) => Vector 4 e -> Vector 3 e
fromHomogenous v = withTensor v $ \x y z w -> scale (vector @3 x y z) (1 / w)
{-# INLINE fromHomogenous #-}

---------------------------------------------------------------------------------------------------
-- | Generate instance of a vector.
genVectorInstance :: Int       -- ^ Size of the vector.
                  -> Name      -- ^ Type of elements.
                  -> Q [Dec]
genVectorInstance size = genTensorInstance (N.fromList [size])