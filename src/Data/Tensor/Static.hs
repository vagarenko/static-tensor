{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE TypeSynonymInstances    #-}           
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE KindSignatures          #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE FlexibleContexts        #-}           
{-# LANGUAGE ScopedTypeVariables     #-}          
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeInType              #-}
{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE TypeFamilyDependencies  #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE InstanceSigs            #-}
{-# LANGUAGE NoStarIsType            #-}
{-# LANGUAGE BangPatterns            #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Tensor.Static
-- Copyright   :  (C) 2017 Alexey Vagarenko
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Alexey Vagarenko (vagarenko@gmail.com)
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------

module Data.Tensor.Static (
    -- * Tensor class
      IsTensor(..)
    , Tensor(..)
    , TensorConstructor
    , PositiveDims
    -- * Construction
    , fill
    , zero
    , enumFromN
    , EnumFromN
    , enumFromStepN
    , EnumFromStepN
    , generate
    , Generate
    -- * Tensor shape
    , dimensions
    , elemsNumber
    , subtensorsElemsNumbers
    , ElemsNumber
    , SubtensorsElemsNumbers
    , FlattenIndex
    , AllIndexes
    , NatsFromTo
    , NormalizeDims
    -- * Modifying tensors
    , withTensor
    , add
    , Add
    , diff
    , Diff
    , scale
    , Scale
    -- ** Concatenation
    , cons
    , Cons
    , ConsSubtensorDims
    , DimsAfterCons
    , snoc
    , Snoc
    , SnocSubtensorDims
    , DimsAfterSnoc
    , append
    , Append
    , DimsAfterAppend
    -- ** Removing slices
    , remove
    , Remove
    , DimsAfterRemove
    -- * Conversion
    , NestedList
    , toNestedList
    , ToNestedList
    -- * Tensor elements
    , tensorElem
    , TensorElem
    -- * Subtensors
    , Subtensor
    , SubtensorStartIndex
    , SubtensorDims
    , subtensor
    , SubtensorCtx
    , getSubtensor
    , GetSubtensor
    , getSubtensorElems
    , GetSubtensorElems
    , setSubtensor
    , SetSubtensor
    , setSubtensorElems
    , SetSubtensorElems
    , mapSubtensorElems
    , MapSubtensorElems
    -- * Slices
    , SliceEndIndex
    , ElemsInSlice
    , slice
    , Slice
    , getSlice
    , GetSlice
    , getSliceElems
    , GetSliceElems
    , setSlice
    , SetSlice
    , setSliceElems
    , SetSliceElems
    , mapSliceElems
    , MapSliceElems
    -- * Constraints for instances
    -- Tensors are instances of 'MonoFunctor', 'MonoFoldable', 'MonoTraversable', 'MonoZip'
    -- with the following constraints:
    , MonoFunctorCtx
    , MonoFoldableCtx
    , MonoTraversableCtx
    , MonoZipCtx
    -- * Pointers
    , unsafeWithTensorPtr
) where

import Control.Lens                 (Lens', lens, Each(..), traversed)
import Data.Containers              (MonoZip(..))
import Data.Function.NAry           (NAry, ApplyNAry(..))
import Data.Kind                    (Type, Constraint)
import Data.List                    (intersperse)
import Data.List.Split              (chunksOf)
import Data.MonoTraversable         (MonoFunctor(..), MonoFoldable(..), MonoTraversable(..), Element)
import Data.Proxy                   (Proxy(..))
import Data.List.Singletons         (Tail, Product, Length)
import Data.Type.Equality           (type (==))
import Data.Type.Bool               (If, type (&&))
import Foreign.Storable             (Storable(..))
import Foreign.Ptr                  (Ptr, castPtr)
import Foreign.Marshal.Utils        (with)
import GHC.TypeLits                 (Nat, KnownNat, natVal, type (+), type (-), type (<=?), type (*), TypeError, ErrorMessage(..))
import Type.List                    (MkCtx, DemoteWith(..), KnownNats(..))
import qualified Data.List.Unrolled as U

---------------------------------------------------------------------------------------------------
-- | Version of 'natVal' with explicit type application.
natVal' :: forall (n :: Nat). (KnownNat n) => Int
natVal' = fromInteger $ natVal (Proxy @n)
{-# INLINE natVal' #-}

---------------------------------------------------------------------------------------------------
-- | Check if all dimensions are greater than 0.
type family PositiveDims (dims :: [Nat]) :: Constraint where
    PositiveDims '[]       = ()
    PositiveDims (d ': ds) = PositiveDims' (1 <=? d) ds

type family PositiveDims' (b :: Bool) (dims :: [Nat]) :: Constraint where
    PositiveDims' 'True  ds = PositiveDims ds
    PositiveDims' 'False _  = TypeError ('Text "Tensor must have positive dimensions.")

-- | Convert multidimentional @index@ in tensor of shape @dims@ to flat index.
--   @index@ parameter must have the same length as @dims@.
--
-- >>> :kind! FlattenIndex '[1,1,1] '[2,3,4]
-- FlattenIndex '[1,1,1] '[2,3,4] :: Nat
-- = 17
type FlattenIndex (index :: [Nat]) (dims :: [Nat]) = FlattenIndex' index (SubtensorsElemsNumbers dims)

type family FlattenIndex' (index :: [Nat]) (elemNumbers :: [Nat]) :: Nat where
    FlattenIndex' '[]       '[]       = 0
    FlattenIndex' (i ': is) '[]       = TypeError ('Text "FlattenIndex: Too many dimensions in the index for subtensor.")
    FlattenIndex' '[]       (n ': ns) = TypeError ('Text "FlattenIndex: Not enough dimensions in the index for subtensor.")
    FlattenIndex' (i ': is) (n ': ns) = i * n + FlattenIndex' is ns

-- | Remove unit dimentions, i.e. dimensions with size @1@.
-- 
-- >>> :kind! NormalizeDims '[2, 1, 3]
-- '[2, 3]
type family NormalizeDims (dims :: [Nat]) :: [Nat] where
    NormalizeDims '[]       = '[]
    NormalizeDims (1 ': xs) = NormalizeDims xs
    NormalizeDims (x ': xs) = x ': NormalizeDims xs

-- | Sequence of all indexes in a tensor of shape @dims@.
--
-- >>> :kind! AllIndexes '[2,3,4]
-- AllIndexes '[2,3,4] :: [[Nat]]
-- = '['[0, 0, 0], '[0, 0, 1], '[0, 0, 2], '[0, 0, 3], '[0, 1, 0],
--     '[0, 1, 1], '[0, 1, 2], '[0, 1, 3], '[0, 2, 0], '[0, 2, 1],
--     '[0, 2, 2], '[0, 2, 3], '[1, 0, 0], '[1, 0, 1], '[1, 0, 2],
--     '[1, 0, 3], '[1, 1, 0], '[1, 1, 1], '[1, 1, 2], '[1, 1, 3],
--     '[1, 2, 0], '[1, 2, 1], '[1, 2, 2], '[1, 2, 3]]
type AllIndexes (dims :: [Nat]) = Sequence (IndexesRanges dims)

-- | Type-level 'sequence'.
type family Sequence (xss :: [[k]]) :: [[k]] where
    Sequence '[]       = '[ '[] ]
    Sequence (x ': xs) = Sequence' x (Sequence xs)

type family Sequence' (xs :: [k]) (yss :: [[k]]) :: [[k]] where
    Sequence' '[]       _  = '[]
    Sequence' (x ': xs) ys = Sequence'' x ys ++ Sequence' xs ys

type family Sequence'' (x :: k) (yss :: [[k]]) :: [[k]] where
    Sequence'' _ '[]       = '[]
    Sequence'' x (y ': ys) = '[ x ': y ] ++ Sequence'' x ys

-- | Append two type-level lists.
infixr 5 ++
type family (++) (xs :: [k]) (ys :: [k]) :: [k] where
    '[]       ++ ys = ys
    (x ': xs) ++ ys = x ': (xs ++ ys)

-- | Ranges for each dimension of an index of the tensor.
-- 
-- >>> :kind! IndexesRanges '[2,3,4]
-- IndexesRanges '[2,3,4] :: [[Nat]]
-- = '['[0, 1], '[0, 1, 2], '[0, 1, 2, 3]]
type family IndexesRanges (dims :: [Nat]) :: [[Nat]] where
    IndexesRanges '[]       = '[]
    IndexesRanges (d ': ds) = IndexesRanges' (d ': ds) (1 <=? d)

type family IndexesRanges' (dims :: [Nat]) (dimPositive :: Bool) :: [[Nat]] where
    IndexesRanges' (d ': ds) 'True  = NatsFromTo 0 (d - 1) ': IndexesRanges ds
    IndexesRanges' (d ':  _) 'False =
        TypeError ('Text "IndexesRanges: Tensor has non-positive dimension: " ':<>: 'ShowType d)

-- | Generate range of naturals starting from @from@ param inclusive and up to @to@ param inclusive.
type NatsFromTo (from :: Nat) (to :: Nat) = NatsFromTo' from to (from <=? to)

type family NatsFromTo' (from :: Nat) (to :: Nat) (fromLTEto :: Bool) :: [Nat] where
    NatsFromTo' _ _ 'False = '[]
    NatsFromTo' f t 'True  = f ': NatsFromTo' (f + 1) t (f + 1 <=? t)

---------------------------------------------------------------------------------------------------
-- | Data family of unboxed tensors. Dimensions of a tensor are represented as type-level list of 
--   naturals. For instance, @Tensor [3] Float@ is a vector of 3 'Float' elements; @Tensor [4,3] Double@ 
--   is a matrix with 4 rows 3 columns of 'Double' and so on.
class (PositiveDims dims, KnownNats dims) => IsTensor (dims :: [Nat]) e where
    {-# MINIMAL tensor, unsafeFromList, toList #-}

    -- | Tensor data constructor for given size and element type.
    data Tensor dims e :: Type

    -- | Alias for a concrete tensor data constructor.
    -- 
    -- >>> tensor @[2,2] @Int 0 1 2 3
    -- Tensor'2'2 [[0,1],[2,3]]
    tensor :: TensorConstructor dims e

    -- | Build tensor from the list. The list must contain at least 'length' elements or method will throw an exception.
    unsafeFromList :: [e] -> Tensor dims e

    -- | Convert tensor to list.
    toList :: Tensor dims e -> [] e

-- | Type of a tensor data constructor.
--
-- >>> :kind! TensorConstructor '[2,2] Float
-- TensorConstructor '[2,2] Float :: *
-- = Float -> Float -> Float -> Float -> Tensor '[2, 2] Float
type TensorConstructor (dims :: [Nat]) (e :: Type) = NAry (ElemsNumber dims) e (Tensor dims e)

-- | Number of elements in a tensor.
type family ElemsNumber (dims :: [Nat]) :: Nat where
    ElemsNumber '[]       = 1
    ElemsNumber (d ': ds) = d * ElemsNumber ds

---------------------------------------------------------------------------------------------------
-- | Scalar is a degenerate tensor.
instance IsTensor '[] e where
    newtype Tensor '[] e = Scalar e
    
    tensor = Scalar
    {-# INLINE tensor #-}

    unsafeFromList (a:_) = Scalar a
    unsafeFromList _     = error "Not enough elements to build a Tensor of shape []."
    {-# INLINE unsafeFromList #-}

    toList (Scalar a) = [a]
    {-# INLINE toList #-}

---------------------------------------------------------------------------------------------------
instance {-# OVERLAPPABLE #-}
         ( Show (NestedList (Length dims) e)
         , IsTensor dims e
         , ToNestedListWrk dims e
         , KnownNats dims
         ) =>
         Show (Tensor dims e)
    where
    show t = "Tensor'" ++ dims ++ " " ++ show (toNestedList t)
        where
            dims = concat $ intersperse "\'" $ map show (dimensions @dims)
    {-# INLINE show #-}

instance {-# OVERLAPPING #-} (Show e) => Show (Tensor '[] e) where
    show (Scalar e) = "Scalar " ++ show e
    {-# INLINE show #-}

---------------------------------------------------------------------------------------------------
-- | Pass tensor elements to a function.
--
-- >>> withTensor (matrix @2 @2 @Float 0 1 2 3) (\a b c d -> a * d - b * c)
-- -2.0
--
-- >>> withTensor (vector @2 @Float 3 4) (\x y -> sqrt $ x * x + y * y)
-- 5.0
withTensor :: forall dims e r.
    ( IsTensor dims e
    , ApplyNAry (ElemsNumber dims) e r
    )
    => Tensor dims e                    -- ^ The tensor.
    -> (NAry (ElemsNumber dims) e r)    -- ^ Function with number of params equal to number of tensor elements.
    -> r
withTensor t f = applyNAry @(ElemsNumber dims) @e @r f (toList t)
{-# INLINE withTensor #-}

-- | Dimensions of a tensor.
dimensions :: forall (dims :: [Nat]). (KnownNats dims) => [Int]
dimensions = natsVal @dims
{-# INLINE dimensions #-}

-- | Number of elements in a tensor.
elemsNumber :: forall (dims :: [Nat]). (KnownNat (ElemsNumber dims)) => Int
elemsNumber = natVal' @(ElemsNumber dims)
{-# INLINE elemsNumber #-}

-- | Number of elements of all subtensors of a tensor.
--
-- >>> subtensorsElemsNumbers @[2,3,4]
-- [12,4,1]
-- 
-- >>> subtensorsElemsNumbers @[4,4]
-- [4,1]
subtensorsElemsNumbers :: forall (dims :: [Nat]). (KnownNats (SubtensorsElemsNumbers dims)) => [Int]
subtensorsElemsNumbers = natsVal @(SubtensorsElemsNumbers dims)
{-# INLINE subtensorsElemsNumbers #-}

-- | Add two tensors element-wise.
add :: (Add dims e) => Tensor dims e -> Tensor dims e -> Tensor dims e
add = ozipWith (+)
{-# INLINE add #-}

-- | Constraints for 'add'.
type Add (dims :: [Nat]) e =
    ( IsTensor dims e
    , Num e
    , U.ZipWith (ElemsNumber dims)
    , U.Zip (ElemsNumber dims)
    , U.Unzip (ElemsNumber dims)
    , U.Map (ElemsNumber dims)
    )

-- | Substract two tensors element-wise.
diff :: (Diff dims e) => Tensor dims e -> Tensor dims e -> Tensor dims e
diff = ozipWith (-)
{-# INLINE diff #-}

-- | Constraints for 'diff'.
type Diff (dims :: [Nat]) e =
    ( IsTensor dims e
    , Num e
    , U.ZipWith (ElemsNumber dims)
    , U.Zip (ElemsNumber dims)
    , U.Unzip (ElemsNumber dims)
    , U.Map (ElemsNumber dims)
    )

-- | Multiply every element of a tensor by given value.
scale :: (Scale dims e) => Tensor dims e -> e -> Tensor dims e
scale t k = omap (*k) t
{-# INLINE scale #-}

-- | Constraints for 'scale'.
type Scale (dims :: [Nat]) e =
    ( IsTensor dims e
    , Num e
    , U.Map (ElemsNumber dims)
    )

-- | Tensor filled with given elements.
fill :: forall (dims :: [Nat]) e. (Fill dims e) => e -> Tensor dims e
fill = unsafeFromList . U.replicate @(ElemsNumber dims)
{-# INLINE fill #-}

-- | Constraints for 'fill'.
type Fill (dims :: [Nat]) e = (IsTensor dims e, U.Replicate (ElemsNumber dims))

-- | Tensor filled with zeros.
zero :: (Fill dims e, Num e) => Tensor dims e
zero = fill 0
{-# INLINE zero #-}

-- | Tensor which elements are enumeration starting from given value.
enumFromN :: forall (dims :: [Nat]) e.
    (EnumFromN dims e)
    => e                       -- ^ Starting value.
    -> Tensor dims e
enumFromN = unsafeFromList . U.enumFromN @(ElemsNumber dims)
{-# INLINE enumFromN #-}

-- | Constraints for 'enumFromN' function.
type EnumFromN (dims :: [Nat]) e =
    ( IsTensor dims e
    , U.EnumFromN (ElemsNumber dims)
    , Num e
    )

-- | Tensor which elements are enumeration starting from given value with given step.
enumFromStepN :: forall (dims :: [Nat]) e.
    (EnumFromStepN dims e)
    => e                   -- ^ Starting value.
    -> e                   -- ^ Step.
    -> Tensor dims e
enumFromStepN a = unsafeFromList . U.enumFromStepN @(ElemsNumber dims) a
{-# INLINE enumFromStepN #-}

-- | Constraints for 'enumFromStepN' function.
type EnumFromStepN (dims :: [Nat]) e =
    ( IsTensor dims e
    , U.EnumFromStepN (ElemsNumber dims)
    , Num e
    )

---------------------------------------------------------------------------------------------------
-- | Generate a tensor by applying the function to each index.
--   @ctx@ type parameter is a producer of constraint of kind @kctx@ for each index. See 'TypeLits.List.MkCtx' for more info.
--
-- >>> import Data.Singletons
-- >>> type Ctx (dims :: [Nat]) (index :: [Nat]) = KnownNat (FlattenIndex index dims); $(genDefunSymbols [''Ctx])
-- >>> generate @[2,3,4] @Int @([Nat] ~> Constraint) @(CtxSym1 [2,3,4]) $ \(Proxy :: Proxy index) -> fromIntegral $ natVal (Proxy @(FlattenIndex index [2,3,4]))
-- Tensor'2'3'4 [[[0,1,2,3],[4,5,6,7],[8,9,10,11]],[[12,13,14,15],[16,17,18,19],[20,21,22,23]]]
generate :: forall (dims :: [Nat]) (e :: Type) (kctx :: Type) (ctx :: kctx).
    (Generate dims e kctx ctx)
    =>  (forall (index :: [Nat]).
        (MkCtx [Nat] kctx ctx index)
        => Proxy index
        -> e
        ) -- ^ Generator function that takes index as type parameter will be called for each index.
    -> Tensor dims e
generate f = unsafeFromList (demoteWith @[Nat] @kctx @ctx @(AllIndexes dims) f)
{-# INLINE generate #-}

-- | Constraints for 'generate' function.
type Generate (dims :: [Nat]) (e :: Type) (kctx :: Type) (ctx :: kctx) =
    ( IsTensor dims e
    , DemoteWith [Nat] kctx ctx (AllIndexes dims)
    )

---------------------------------------------------------------------------------------------------
-- | Nested list of given @depth@.
--
-- >>> :kind! NestedList 3 Float
-- [[[Float]]]
--
-- >>> :kind! NestedList 2 Float
-- [[Float]]
type family NestedList (depth :: Nat) (e :: Type) :: Type where
    NestedList 0 e = e
    NestedList n e = [NestedList (n - 1) e]

-- | Convert tensor to nested list.
toNestedList :: forall dims e. (ToNestedList dims e)
    => Tensor dims e                    -- ^
    -> NestedList (Length dims) e
toNestedList = toNestedListWrk @dims @e . toList
{-# INLINE toNestedList #-}

-- | Constraints for 'toNestedList' function.
type ToNestedList (dims :: [Nat]) e = (IsTensor dims e, ToNestedListWrk dims e)

-- | Worker for 'toNestedList'.
class ToNestedListWrk (dims :: [Nat]) e where
    toNestedListWrk :: [e] -> NestedList (Length dims) e

instance ToNestedListWrk '[] e where
    toNestedListWrk = head

instance ToNestedListWrk '[x] e where
    toNestedListWrk = id
    {-# INLINE toNestedListWrk #-}

instance ( ToNestedListWrk (xx ': xs) e
         , KnownNat (Product (xx ': xs))
         , NestedList (Length (x ': xx ': xs)) e ~ [NestedList (Length (xx ': xs)) e]
         ) => 
         ToNestedListWrk (x ': xx ': xs) e
    where
    toNestedListWrk xs = map (toNestedListWrk @(xx ': xs)) $ chunksOf (natVal' @(Product (xx ': xs))) xs
    {-# INLINABLE toNestedListWrk #-}

---------------------------------------------------------------------------------------------------
-- Subtensors
---------------------------------------------------------------------------------------------------
-- | Subtensor at @index@ of a tensor of shape @dims@.
--
-- >>> :kind! Subtensor '[] '[2,3,4] Float
-- Subtensor '[] '[2,3,4] Float :: *
-- = Tensor '[2, 3, 4] Float
--
-- >>> :kind! Subtensor '[0] '[2,3,4] Float
-- Subtensor '[0] '[2,3,4] Float :: *
-- = Tensor '[3, 4] Float
--
-- >>> :kind! Subtensor '[0,0] '[2,3,4] Float
-- Subtensor '[0,0] '[2,3,4] Float :: *
-- = Tensor '[4] Float
--
-- >>> :kind! Subtensor '[0,0,0] '[2,3,4] Float
-- Subtensor '[0,0,0] '[2,3,4] Float :: *
-- = Tensor '[] Float
type Subtensor index dims e = Tensor (NormalizeDims (SubtensorDims index dims)) e

-- | Number of elements of all subtensors of a tensor with given shape @dims@.
--
-- >>> :kind! SubtensorsElemsNumbers '[2,3,4]
-- SubtensorsElemsNumbers '[2,3,4] :: [Nat]
-- = '[12, 4, 1]
--
-- >>> :kind! SubtensorsElemsNumbers '[4,4]
-- SubtensorsElemsNumbers '[4,4] :: [Nat]
-- = '[4, 1]
type SubtensorsElemsNumbers (dims :: [Nat]) = Tail (SubtensorsElemsNumbers' dims)

type family SubtensorsElemsNumbers' (dims :: [Nat]) :: [Nat] where
    SubtensorsElemsNumbers' '[]       = '[1]
    SubtensorsElemsNumbers' (d ': ds) = SubtensorsElemsNumbers'' d (SubtensorsElemsNumbers' ds)

type family SubtensorsElemsNumbers'' (dim :: Nat) (dims :: [Nat]) :: [Nat] where
    SubtensorsElemsNumbers'' d (q ': qs) = d * q ': q ': qs

-- | Shape of a subtensor of tensor of shape @dims@ located at @index@. Resulting shape is not normalized.
--
-- >>> :kind! SubtensorDims '[0] '[2,3,4]
-- SubtensorDims '[0] '[2,3,4] :: [Nat]
-- = '[1, 3, 4]
-- 
-- >>> :kind! SubtensorDims '[0,0] '[2,3,4]
-- SubtensorDims '[0,0] '[2,3,4] :: [Nat]
-- = '[1, 1, 4]
type family SubtensorDims (index :: [Nat]) (dims :: [Nat]) :: [Nat] where
    SubtensorDims '[]        ds       = ds
    SubtensorDims (_ ': _ ) '[]       = TypeError ('Text "SubtensorDims: Too many dimensions in the index for subtensor.")
    SubtensorDims (i ': is) (d ': ds) = 
        If  (i <=? d - 1)
            (1 ': SubtensorDims is ds)
            (TypeError 
                ('Text "SubtensorDims: Index "
                ':<>: 'ShowType i
                ':<>: 'Text " is outside of the range of dimension [0.."
                ':<>: 'ShowType (d - 1)
                ':<>: 'Text "]."))

-- | Index of the first element of the subtensor of the tensor of shape @dims@ at @index@.
--   This function returns index with number of dimensions equal to number of dimensions of the 
--   tensor.
--
-- >>> :kind! SubtensorStartIndex '[1] '[2,3,4]
-- SubtensorStartIndex '[1] '[2,3,4] :: [Nat]
-- = '[1, 0, 0]
--
-- >>> :kind! SubtensorStartIndex '[0,1] '[2,3,4]
-- SubtensorStartIndex '[0,1] '[2,3,4] :: [Nat]
-- = '[0, 1, 0]
--
-- >>> :kind! SubtensorStartIndex '[1,1] '[2,3,4]
-- SubtensorStartIndex '[1,1] '[2,3,4] :: [Nat]
-- = '[1, 1, 0]
type family SubtensorStartIndex (index :: [Nat]) (dims :: [Nat]) :: [Nat] where
    SubtensorStartIndex '[]       '[]       = '[]
    SubtensorStartIndex (i ': is) '[]       = TypeError ('Text "SubtensorStartIndex: Too many dimensions in the index for subtensor.")
    SubtensorStartIndex '[]       (d ': ds) = 0 ': SubtensorStartIndex '[] ds
    SubtensorStartIndex (i ': is) (d ': ds) = 
        If  (i <=? d - 1)
            (i ': SubtensorStartIndex  is ds)
            (TypeError 
                ('Text "SubtensorStartIndex: Index "
                ':<>: 'ShowType i
                ':<>: 'Text " is outside of the range of dimension [0.."
                ':<>: 'ShowType (d - 1)
                ':<>: 'Text "]."))

---------------------------------------------------------------------------------------------------
-- | Extract subtensor at given index.
getSubtensor :: forall (index :: [Nat]) (dims :: [Nat]) e. 
    (GetSubtensor index dims e)
    => Tensor dims e            -- ^
    -> Subtensor index dims e
getSubtensor = getSlice @(SubtensorStartIndex index dims) @(SubtensorDims index dims) @dims @e
{-# INLINE getSubtensor #-}

-- | Constraint for 'getSubtensor' function.
type GetSubtensor index dims e =
    ( GetSlice (SubtensorStartIndex index dims) (SubtensorDims index dims) dims e
    )

---------------------------------------------------------------------------------------------------
-- | Extract elements of subtensor at given index.
--   Like 'getSubtensor', but without building actual subtensor.
getSubtensorElems :: forall (index :: [Nat]) (dims :: [Nat]) e. 
    (GetSubtensorElems index dims e)
    => Tensor dims e        -- ^ 
    -> [e]
getSubtensorElems = getSliceElems @(SubtensorStartIndex index dims) @(SubtensorDims index dims) @dims @e
{-# INLINE getSubtensorElems #-}

-- | Constraint for 'getSubtensorElems' function.
type GetSubtensorElems index dims e =
    GetSliceElems (SubtensorStartIndex index dims) (SubtensorDims index dims) dims e

---------------------------------------------------------------------------------------------------
-- | Set subtensor at given index.
setSubtensor :: forall (index :: [Nat]) (dims :: [Nat]) e.
    (SetSubtensor index dims e)
    => Tensor dims e               -- ^ The tensor.
    -> Subtensor index dims e      -- ^ New subtensor.
    -> Tensor dims e
setSubtensor = setSlice @(SubtensorStartIndex index dims) @(SubtensorDims index dims) @dims @e
{-# INLINE setSubtensor #-}

-- | Constraint for 'setSubtensor' function.
type SetSubtensor index dims e =
    SetSlice (SubtensorStartIndex index dims) (SubtensorDims index dims) dims e

---------------------------------------------------------------------------------------------------
-- | Like 'setSubtensor' but takes a list of elements instead of a tensor.
--   Returns @Nothing@ if list has not enough elements.
setSubtensorElems :: forall (index :: [Nat]) (dims :: [Nat]) e.
    (SetSubtensorElems index dims e)
    => Tensor dims e            -- ^ The tensor.
    -> [e]                      -- ^ New elements of the subtensor.
    -> Maybe (Tensor dims e)
setSubtensorElems = setSliceElems @(SubtensorStartIndex index dims) @(SubtensorDims index dims) @dims @e
{-# INLINE setSubtensorElems #-}

-- | Constraint for 'setSubtensorElems' function.
type SetSubtensorElems index dims e =
    SetSliceElems (SubtensorStartIndex index dims) (SubtensorDims index dims) dims e

---------------------------------------------------------------------------------------------------
-- | Modify subtensor elements with a function.
mapSubtensorElems :: forall (index :: [Nat]) (dims :: [Nat]) e.
    (MapSubtensorElems index dims e)
    => Tensor dims e        -- ^ The tensor.
    -> (e -> e)             -- ^ The mapping function.
    -> Tensor dims e
mapSubtensorElems = mapSliceElems @(SubtensorStartIndex index dims) @(SubtensorDims index dims) @dims @e
{-# INLINE mapSubtensorElems #-}

-- | Constraints for 'mapSubtensorElems'.
type MapSubtensorElems index dims e =
    MapSliceElems (SubtensorStartIndex index dims) (SubtensorDims index dims) dims e

---------------------------------------------------------------------------------------------------
-- | Lens for subtensor at given index.
--
-- >>> let t = enumFromN @[2,3,4] @Int 0
-- >>> t
-- Tensor'2'3'4 [[[  0,  1,  2,  3]
--               ,[  4,  5,  6,  7]
--               ,[  8,  9, 10, 11]]
--              ,[[ 12, 13, 14, 15]
--               ,[ 16, 17, 18, 19]
--               ,[ 20, 21, 22, 23]]]
-- >>> t ^. subtensor @'[0]
-- Tensor'3'4    [[  0,  1,  2,  3]
--               ,[  4,  5,  6,  7]
--               ,[  8,  9, 10, 11]]
-- >>> t ^. subtensor @'[1]
-- Tensor'3'4    [[ 12, 13, 14, 15]
--               ,[ 16, 17, 18, 19]
--               ,[ 20, 21, 22, 23]]
-- >>> t ^. subtensor @'[0,0]
-- Tensor'4       [  0,  1,  2,  3]
-- >>> t ^. subtensor @'[1,0]
-- Tensor'4       [ 12, 13, 14, 15]
subtensor :: forall (index :: [Nat]) (dims :: [Nat]) e.
    (SubtensorCtx index dims e)
    => Lens' (Tensor dims e) (Subtensor index dims e)       -- ^
subtensor = lens (getSubtensor @index @dims @e) (setSubtensor @index @dims @e)
{-# INLINE subtensor #-}

-- | Constraint for 'subtensor' function.
type SubtensorCtx index dims e =
    ( GetSubtensor index dims e
    , SetSubtensor index dims e)

---------------------------------------------------------------------------------------------------
-- | Lens for an element of a tensor.
--
-- >>> let t = enumFromN @[2,3,4] @Int 0
-- >>> t
-- Tensor'2'3'4 [[[  0,  1,  2,  3]
--               ,[  4,  5,  6,  7]
--               ,[  8,  9, 10, 11]]
--              ,[[ 12, 13, 14, 15]
--               ,[ 16, 17, 18, 19]
--               ,[ 20, 21, 22, 23]]]
-- >>> t ^. tensorElem @[1,1,1]
-- 17
-- >>> set (tensorElem @[1,1,1]) 0 t
-- Tensor'2'3'4 [[[  0,  1,  2,  3]
--               ,[  4,  5,  6,  7]
--               ,[  8,  9, 10, 11]]
--              ,[[ 12, 13, 14, 15]
--               ,[ 16,  0, 18, 19]
--               ,[ 20, 21, 22, 23]]]
tensorElem :: forall (index :: [Nat]) (dims :: [Nat]) e.
    (TensorElem index dims e)
    => Lens' (Tensor dims e) e      -- ^
tensorElem = subtensor @index @dims @e . (lens (\(Scalar a) -> a) (\_ b -> Scalar b))
{-# INLINE tensorElem #-}

-- | Constraint for 'tensorElem' function.
type TensorElem index dims e =
    ( SubtensorCtx index dims e
    , NormalizeDims (SubtensorDims index dims) ~ '[]
    )

---------------------------------------------------------------------------------------------------
-- Slices
---------------------------------------------------------------------------------------------------
-- | Index of the end of the slice of the tensor. @startIndex@ parameter is the starting index of the slice,
--   @sliceDims@ is the shape of the slice, @dims@ is the shape of the tensor.
--   The slice must be contained inside the tensor.
--   All dimensions of the slice must be positive.
--   @startIndex@, @sliceDims@ and @dims@ must have the same length. If you want to get slice of lower rank than
--   the tensor's, set one or more dimensions in @sliceDims@ to @1@.
--
-- >>> :kind! SliceEndIndex '[0,0,0] '[2,2,2] '[2,3,4]
-- SliceEndIndex '[0,0,0] '[2,2,2] '[2,3,4] :: [Nat]
-- = '[1, 1, 1]
--
-- >>> :kind! SliceEndIndex '[1,1,0] '[1,2,4] '[2,3,4]
-- SliceEndIndex '[1,1,0] '[1,2,4] '[2,3,4] :: [Nat]
-- = '[1, 2, 3]
type family SliceEndIndex (startIndex :: [Nat]) (sliceDims :: [Nat]) (dims :: [Nat]) :: [Nat] where
    SliceEndIndex '[]         '[]         '[]       = '[]
    SliceEndIndex '[]         '[]         (d ': ds) = TypeError ('Text "SliceEndIndex: Slice and its starting index have not enough dimensions.")
    SliceEndIndex '[]         (sd ': sds) '[]       = TypeError ('Text "SliceEndIndex: Slice has too many dimensions.")
    SliceEndIndex '[]         (sd ': sds) (d ': ds) = TypeError ('Text "SliceEndIndex: Starting index of the slice has not enough dimensions.")
    SliceEndIndex (si ': sis) '[]         '[]       = TypeError ('Text "SliceEndIndex: Starting index of the slice has too many dimensions.")
    SliceEndIndex (si ': sis) '[]         (d ': ds) = TypeError ('Text "SliceEndIndex: Slice has not enough dimensions.")
    SliceEndIndex (si ': sis) (sd ': sds) '[]       = TypeError ('Text "SliceEndIndex: Slice and its starting index have too many dimensions.")
    SliceEndIndex (si ': sis) (sd ': sds) (d ': ds) = SliceEndIndex' (si ': sis) (sd ': sds) (d ': ds) (1 <=? sd)

type family SliceEndIndex' (startIndex :: [Nat]) (sliceDims :: [Nat]) (dims :: [Nat]) (sliceDimPositive :: Bool) :: [Nat] where
    SliceEndIndex' (si ': sis) (sd ': sds) (d ': ds) 'True  = SliceEndIndex'' (si ': sis) (sd ': sds) (d ': ds) (si + sd <=? d)
    SliceEndIndex'           _ (sd ':   _)         _ 'False =
        TypeError ('Text "SliceEndIndex: Slice has non-positive dimension: " ':<>: 'ShowType sd)

type family SliceEndIndex'' (startIndex :: [Nat]) (sliceDims :: [Nat]) (dims :: [Nat]) (sliceDimInside :: Bool) :: [Nat] where
    SliceEndIndex'' (si ': sis) (sd ': sds) (d ': ds) 'True  = (si + sd - 1 ': SliceEndIndex sis sds ds)
    SliceEndIndex'' (si ': sis) (sd ': sds) (d ': ds) 'False =
        (TypeError
            (      'Text "SliceEndIndex: Slice dimension is outside of the tensor. It starts at "
             ':<>: 'ShowType si
             ':<>: 'Text " and ends at "
             ':<>: 'ShowType (si + sd - 1)
             ':<>: 'Text " which is outside of the range of the tensor's dimension [0.."
             ':<>: 'ShowType (d - 1)
             ':<>: 'Text "]."))

-- | Check each element of the tensor of shape @dims@ if it is inside
--   the slice from @startIndex@ of shape @sliceDims@.
--   The slice must be contained inside the tensor.
--   All dimensions of the slice must be positive.
--   @startIndex@, @sliceDims@ and @dims@ must have the same length.
--
-- >>> :kind! ElemsInSlice '[0,0,0] '[2,2,2] '[2,3,4]
-- ElemsInSlice '[0,0,0] '[2,2,2] '[2,3,4] :: [Bool]
-- = '['True, 'True, 'False, 'False, 'True, 'True, 'False, 'False,
--     'False, 'False, 'False, 'False, 'True, 'True, 'False, 'False,
--     'True, 'True, 'False, 'False, 'False, 'False, 'False, 'False]
--
-- >>> :kind! ElemsInSlice '[1,1,0] '[1,2,4] '[2,3,4]
-- ElemsInSlice '[1,1,0] '[1,2,4] '[2,3,4] :: [Bool]
-- = '['False, 'False, 'False, 'False, 'False, 'False, 'False, 'False,
--     'False, 'False, 'False, 'False, 'False, 'False, 'False, 'False,
--     'True, 'True, 'True, 'True, 'True, 'True, 'True, 'True]
type ElemsInSlice (startIndex :: [Nat]) (sliceDims :: [Nat]) (dims :: [Nat]) =
    ElemsInSlice' startIndex (SliceEndIndex startIndex sliceDims dims) (AllIndexes dims)

-- | Map over @indexes@.
type family ElemsInSlice' (startIndex :: [Nat]) (endIndex :: [Nat]) (indexes :: [[Nat]]) :: [Bool] where
    ElemsInSlice' _          _        '[]       = '[]
    ElemsInSlice' startIndex endIndex (i ': is) = ElemsInSlice'' i startIndex endIndex ': ElemsInSlice' startIndex endIndex is

-- | Check if index is inside the slice.
type family ElemsInSlice'' (index :: [Nat]) (startIndex :: [Nat]) (endIndex :: [Nat]) :: Bool where
    ElemsInSlice'' (i ': is) (s ': ss) (e ': es) = s <=? i && i <=? e && ElemsInSlice'' is ss es
    ElemsInSlice'' '[]       '[]       '[]       = 'True

---------------------------------------------------------------------------------------------------
-- | Lens for the slice starting from @startIndex@ of shape @sliceDims@ of the tensor of shape @dims@.
--
-- >>> let t = (enumFromN @[2,3,4] @Int 0)
-- >>> t
-- Tensor'2'3'4 [[[  0,  1,  2,  3]
--               ,[  4,  5,  6,  7]
--               ,[  8,  9, 10, 11]]
--              ,[[ 12, 13, 14, 15]
--               ,[ 16, 17, 18, 19]
--               ,[ 20, 21, 22, 23]]]
-- >>> t ^. slice @[0,0,0] @[2,2,2]
-- Tensor'2'2'2 [[[  0,  1]
--               ,[  4,  5]]
--              ,[[ 12, 13]
--               ,[ 16, 17]]]
-- >>> set (slice @[0,0,0] @[2,2,2]) zero t
-- Tensor'2'3'4 [[[  0,  0,  2,  3]
--               ,[  0,  0,  6,  7]
--               ,[  8,  9, 10, 11]]
--              ,[[  0,  0, 14, 15]
--               ,[  0,  0, 18, 19]
--               ,[ 20, 21, 22, 23]]]
slice :: forall startIndex sliceDims dims e.
    (Slice startIndex sliceDims dims e)
    => Lens' (Tensor dims e) (Tensor (NormalizeDims sliceDims) e)   -- ^ 
slice = lens (getSlice @startIndex @sliceDims @dims @e) (setSlice @startIndex @sliceDims @dims @e)
{-# INLINE slice #-}

-- | Constraints for 'slice' function.
type Slice startIndex sliceDims dims e =
    ( IsTensor dims e
    , IsTensor (NormalizeDims sliceDims) e
    , GetSliceElemsWrk (ElemsInSlice startIndex sliceDims dims)
    , SetSliceElemsWrk (ElemsInSlice startIndex sliceDims dims)
    )

---------------------------------------------------------------------------------------------------
-- | Extract slice of shape @sliceDims@ from a tensor of shape @dims@
--   starting at @startIndex@ for each axis.
getSlice :: forall startIndex sliceDims dims e.
    (GetSlice startIndex sliceDims dims e)
    => Tensor dims e                        -- ^ 
    -> Tensor (NormalizeDims sliceDims) e
getSlice = unsafeFromList . getSliceElems @startIndex @sliceDims @dims @e
{-# INLINE getSlice #-}

-- | Constraints for 'getSlice'.
type GetSlice startIndex sliceDims dims e =
    ( IsTensor dims e
    , IsTensor (NormalizeDims sliceDims) e
    , GetSliceElemsWrk (ElemsInSlice startIndex sliceDims dims)
    )

---------------------------------------------------------------------------------------------------
-- | Same as slice but returns list of elements instead of tensor data type.
getSliceElems :: forall startIndex sliceDims dims e.
    (GetSliceElems startIndex sliceDims dims e)
    => Tensor dims e            -- ^ 
    -> [e]
getSliceElems = getSliceElemsWrk @(ElemsInSlice startIndex sliceDims dims) . toList
{-# INLINE getSliceElems #-}

-- | Constraints for 'getSliceElems'.
type GetSliceElems startIndex sliceDims dims e =
    ( IsTensor dims e
    , GetSliceElemsWrk (ElemsInSlice startIndex sliceDims dims)
    )

-- | Fail when list representation of a tensor has less elements than tensor. This should never happen.
impossible_notEnoughTensorElems :: a
impossible_notEnoughTensorElems =
    error "Impossible happend! Not enough elements in the tensor. Please report this bug."
{-# INLINE impossible_notEnoughTensorElems #-}

-- | Worker function for 'getSliceElems'.
class GetSliceElemsWrk (elemsInSlice :: [Bool]) where
    getSliceElemsWrk :: [e] -> [e]

instance GetSliceElemsWrk '[] where
    getSliceElemsWrk _ = []
    {-# INLINE getSliceElemsWrk #-}

instance (GetSliceElemsWrk xs) => GetSliceElemsWrk ('True ': xs) where
    getSliceElemsWrk []       = impossible_notEnoughTensorElems
    getSliceElemsWrk (x : xs) = x : getSliceElemsWrk @xs xs
    {-# INLINE getSliceElemsWrk #-}

instance (GetSliceElemsWrk xs) => GetSliceElemsWrk ('False ': xs) where
    getSliceElemsWrk []       = impossible_notEnoughTensorElems
    getSliceElemsWrk (_ : xs) = getSliceElemsWrk @xs xs
    {-# INLINE getSliceElemsWrk #-}

---------------------------------------------------------------------------------------------------
-- | Set elements of the slice.
setSlice :: forall startIndex sliceDims dims e.
    (SetSlice startIndex sliceDims dims e)
    => Tensor dims e                            -- ^ The tensor.
    -> Tensor (NormalizeDims sliceDims) e       -- ^ New slice.
    -> Tensor dims e
setSlice t st =
    case setSliceElems @startIndex @sliceDims @dims @e t $ toList st of
        Nothing -> impossible_notEnoughTensorElems
        Just  x -> x
{-# INLINE setSlice #-}

-- | Constraints for 'setSlice'.
type SetSlice startIndex sliceDims dims e =
    ( IsTensor dims e
    , IsTensor (NormalizeDims sliceDims) e
    , SetSliceElemsWrk (ElemsInSlice startIndex sliceDims dims)
    )

---------------------------------------------------------------------------------------------------
-- | Like 'setSlice' but takes a list of elements instead of a tensor.
--   Returns @Nothing@ if list has less than @'ElemsNumber' sliceDims@ elements.
setSliceElems :: forall startIndex sliceDims dims e.
    (SetSliceElems startIndex sliceDims dims e)
    => Tensor dims e                -- ^ The tensor.
    -> [e]                          -- ^ New elements of the slice.
    -> Maybe (Tensor dims e)
setSliceElems t xs = unsafeFromList <$> setSliceElemsWrk @(ElemsInSlice startIndex sliceDims dims) (toList t) xs
{-# INLINE setSliceElems #-}

-- | Constraints for 'setSliceElems'.
type SetSliceElems startIndex sliceDims dims e =
    ( IsTensor dims e
    , SetSliceElemsWrk (ElemsInSlice startIndex sliceDims dims)
    )

-- | Worker function for 'setSliceElems'
class SetSliceElemsWrk (elemsInSlice :: [Bool]) where
    setSliceElemsWrk :: [e] -> [e] -> Maybe [e]

instance SetSliceElemsWrk '[] where
    setSliceElemsWrk _ _ = Just []
    {-# INLINE setSliceElemsWrk #-}

instance (SetSliceElemsWrk xs) => SetSliceElemsWrk ('True ': xs) where
    setSliceElemsWrk []       _        = impossible_notEnoughTensorElems
    setSliceElemsWrk _        []       = Nothing
    setSliceElemsWrk (_ : xs) (y : ys) = (y :) <$> setSliceElemsWrk @xs xs ys
    {-# INLINE setSliceElemsWrk #-}

instance (SetSliceElemsWrk xs) => SetSliceElemsWrk ('False ': xs) where
    setSliceElemsWrk []       _        = impossible_notEnoughTensorElems
    setSliceElemsWrk (x : xs) yss      = (x :) <$> setSliceElemsWrk @xs xs yss
    {-# INLINE setSliceElemsWrk #-}

---------------------------------------------------------------------------------------------------
-- | Modify slice elements with a function.
mapSliceElems :: forall startIndex sliceDims dims e.
    (MapSliceElems startIndex sliceDims dims e)
    => Tensor dims e        -- ^ The tensor.
    -> (e -> e)             -- ^ The mapping function.
    -> Tensor dims e
mapSliceElems t f =
    case setSliceElems @startIndex @sliceDims @dims @e
            t (U.map @(ElemsNumber sliceDims) f (getSliceElems @startIndex @sliceDims @dims @e t))
    of
        Nothing -> impossible_notEnoughTensorElems
        Just  x -> x
{-# INLINE mapSliceElems #-}

-- | Constraints for 'mapSliceElems'.
type MapSliceElems startIndex sliceDims dims e =
    ( IsTensor dims e
    , GetSliceElemsWrk (ElemsInSlice startIndex sliceDims dims)
    , SetSliceElemsWrk (ElemsInSlice startIndex sliceDims dims)
    , U.Map (ElemsNumber sliceDims)
    )

---------------------------------------------------------------------------------------------------
-- | Remove a slice from the tensor. We can only remove slices which have one dimension fewer than the tensor,
--   and which span from borders of the tensor to opposite borders of the tensor (i.e. contain all elements of
--   the tensor in their dimensions).
--
--   @axis@ is the index of dimension in @dims@
--   @indexOnAxis@ is offset along @axis@ that points to the slice to be removed.
--
--   For example, suppose we have tensor @t :: Tensor '[2,3,4] Float@ that is, tensor made of two matrices of 3*4 elements.
--
--   If we want to remove first matrix we write @remove \@0 \@0 t@, if second - @remove \@0 \@1 t@.
--
--   If we want to remove n-th row in all matrices we write @remove \@1 \@n t@.
--
--   If we want to remove n-th column in all matrices we write @remove \@2 \@n t@.
--
-- >>> let t = enumFromN @[2,3,4] @Int 0
-- >>> t
-- Tensor'2'3'4 [[[ 0, 1, 2, 3]
--               ,[ 4, 5, 6, 7]
--               ,[ 8, 9,10,11]]
--              ,[[12,13,14,15]
--               ,[16,17,18,19]
--               ,[20,21,22,23]]]
-- >>> remove @0 @0 t
-- Tensor'1'3'4 [[[12,13,14,15]
--               ,[16,17,18,19]
--               ,[20,21,22,23]]]
-- >>> remove @1 @0 t
-- Tensor'2'2'4 [[[ 4, 5, 6, 7]
--               ,[ 8, 9,10,11]]
--              ,[[16,17,18,19]
--               ,[20,21,22,23]]]
-- >>> remove @2 @0 t
-- Tensor'2'3'3 [[[ 1, 2, 3]
--               ,[ 5, 6, 7]
--               ,[ 9,10,11]]
--              ,[[13,14,15]
--               ,[17,18,19]
--               ,[21,22,23]]]
remove :: forall (axis :: Nat) (indexOnAxis :: Nat) (dims :: [Nat]) e.
    (Remove axis indexOnAxis dims e)
    => Tensor dims e                                    -- ^
    -> Tensor (DimsAfterRemove axis indexOnAxis dims) e
remove = unsafeFromList . removeWrk @(ElemsInSlice (RemoveSliceStartIndex axis indexOnAxis dims) (RemoveSliceDims axis indexOnAxis dims) dims) . toList
{-# INLINE remove #-}

-- | Constraints for 'remove'.
type Remove (axis :: Nat) (indexOnAxis :: Nat) (dims :: [Nat]) e =
    ( IsTensor dims e
    , IsTensor (DimsAfterRemove axis indexOnAxis dims) e
    , RemoveWrk (ElemsInSlice (RemoveSliceStartIndex axis indexOnAxis dims) (RemoveSliceDims axis indexOnAxis dims) dims)
    )

-- | Shape of a tensor @dims@ after removing a slice at @index@ along @axis@.
--
-- >>> :kind! DimsAfterRemove 0 0 [2,3,4]
-- DimsAfterRemove 0 0 [2,3,4] :: [Nat]
-- = '[1, 3, 4]
-- 
-- >>> :kind! DimsAfterRemove 1 0 [2,3,4]
-- DimsAfterRemove 1 0 [2,3,4] :: [Nat]
-- = '[2, 2, 4]
-- 
-- >>> :kind! DimsAfterRemove 2 0 [2,3,4]
-- DimsAfterRemove 2 0 [2,3,4] :: [Nat]
-- = '[2, 3, 3]
type family DimsAfterRemove (axis :: Nat) (index :: Nat) (dims :: [Nat]) :: [Nat] where
    DimsAfterRemove _ _ '[]       = TypeError ('Text "DimsAfterRemove: axis must be in range [0..(number of dimensions in the tensor)].")
    DimsAfterRemove 0 i (d ': ds) =
        If  (i <=? d - 1)
            (d - 1 ': ds)
            (TypeError (
                'Text "DimsAfterRemove: Index "
                ':<>: 'ShowType i
                ':<>: 'Text " is outside of the range of dimension [0.."
                ':<>: 'ShowType (d - 1)
                ':<>: 'Text "]."))
    DimsAfterRemove a i (d ': ds) = d ': DimsAfterRemove (a - 1) i ds

-- | Starting index of the slice to be removed.
--   @axis@ is the index of dimension in @dims@
--   @indexOnAxis@ is offset of the slice along @axis@.
type RemoveSliceStartIndex (axis :: Nat) (indexOnAxis :: Nat) (dims :: [Nat]) = RemoveSliceStartIndex' axis indexOnAxis dims 0

type family RemoveSliceStartIndex' (axis :: Nat) (indexOnAxis :: Nat) (dims :: [Nat]) (n :: Nat) :: [Nat] where
    RemoveSliceStartIndex' _ _ '[]       _ = '[]
    RemoveSliceStartIndex' a i (d ': ds) n =
        If (a == n) i 0 ': RemoveSliceStartIndex' a i ds (n + 1)

-- | Shape of the slice to be removed.
--   @axis@ is the index of dimension in @dims@
--   @indexOnAxis@ is offset of the slice along @axis@.
type family RemoveSliceDims (axis :: Nat) (indexOnAxis :: Nat) (dims :: [Nat]) :: [Nat] where
    RemoveSliceDims _ _ '[]       = TypeError ('Text "RemoveSliceDims: axis must be in range [0..(number of dimensions in the tensor)].")
    RemoveSliceDims 0 i (d ': ds) =
        If  (i <=? d - 1)
            (1 ': ds)
            (TypeError (
                'Text "RemoveSliceDims: Index "
                ':<>: 'ShowType i
                ':<>: 'Text " is outside of the range of dimension [0.."
                ':<>: 'ShowType (d - 1)
                ':<>: 'Text "]."))
    RemoveSliceDims a i (d ': ds) = d ': RemoveSliceDims (a - 1) i ds

-- | Worker function for 'remove'.
class RemoveWrk (elemsInSlice :: [Bool]) where
    removeWrk :: [e] -> [e]

instance RemoveWrk '[] where
    removeWrk _ = []
    {-# INLINE removeWrk #-}

instance (RemoveWrk xs) => RemoveWrk ('False ': xs) where
    removeWrk []       = impossible_notEnoughTensorElems
    removeWrk (x : xs) = x : removeWrk @xs xs
    {-# INLINE removeWrk #-}

instance (RemoveWrk xs) => RemoveWrk ('True ': xs) where
    removeWrk []       = impossible_notEnoughTensorElems
    removeWrk (_ : xs) = removeWrk @xs xs
    {-# INLINE removeWrk #-}

---------------------------------------------------------------------------------------------------
-- | Prepend a subtensor along @axis@ to the tensor with shape @dims@
--
-- >>> cons @0 (enumFromStepN @[3,4] @Int (-1) (-1)) (enumFromN @[2,3,4] 0)
-- Tensor'3'3'4 [[[ -1,  -2,  -3,  -4]
--               ,[ -5,  -6,  -7,  -8]
--               ,[ -9, -10, -11, -12]]
--              ,[[  0,   1,   2,   3]
--               ,[  4,   5,   6,   7]
--               ,[  8,   9,  10,  11]]
--              ,[[ 12,  13,  14,  15]
--               ,[ 16,  17,  18,  19]
--               ,[ 20,  21,  22,  23]]]
--
-- >>> cons @1 (enumFromStepN @[2,4] @Int (-1) (-1)) (enumFromN @[2,3,4] 0)
-- Tensor'2'4'4 [[[ -1,  -2,  -3,  -4]
--               ,[  0,   1,   2,   3]
--               ,[  4,   5,   6,   7]
--               ,[  8,   9,  10,  11]]
--              ,[[ -5,  -6,  -7,  -8]
--               ,[ 12,  13,  14,  15]
--               ,[ 16,  17,  18,  19]
--               ,[ 20,  21,  22,  23]]]
--
-- >>> cons @2 (enumFromStepN @[2,3] @Int (-1) (-1)) (enumFromN @[2,3,4] 0)
-- Tensor'2'3'5 [[[ -1,   0,   1,   2,   3]
--               ,[ -2,   4,   5,   6,   7]
--               ,[ -3,   8,   9,  10,  11]]
--              ,[[ -4,  12,  13,  14,  15]
--               ,[ -5,  16,  17,  18,  19]
--               ,[ -6,  20,  21,  22,  23]]]
cons :: forall (axis :: Nat) (dims :: [Nat]) e.
        (Cons axis dims e) =>
           Tensor (NormalizeDims (ConsSubtensorDims axis dims)) e    -- ^ Subtensor to cons.
        -> Tensor dims e                                             -- ^ Tensor to cons to.
        -> Tensor (DimsAfterCons axis dims) e
cons st t =
        setSlice @(ConsSubtensorStartingIndex dims) @(ConsSubtensorDims axis dims) @(DimsAfterCons axis dims) @e t' st
    where
        t' = setSlice @(ConsTensorStartingIndex axis dims) @dims @(DimsAfterCons axis dims) z t
        z = fill @(DimsAfterCons axis dims) @e (head $ toList t)
{-# INLINE cons #-}

-- | Constraints for 'cons'.
type Cons (axis :: Nat) (dims :: [Nat]) e =
    ( SetSlice (ConsSubtensorStartingIndex dims) (ConsSubtensorDims axis dims) (DimsAfterCons axis dims) e
    , SetSlice (ConsTensorStartingIndex axis dims) dims (DimsAfterCons axis dims) e
    , dims ~ NormalizeDims dims
    , Fill (DimsAfterCons axis dims) e
    )

type family ConsSubtensorStartingIndex (dims :: [Nat]) :: [Nat] where
    ConsSubtensorStartingIndex '[]       = '[]
    ConsSubtensorStartingIndex (_ ': ds) = 0 ': ConsSubtensorStartingIndex ds

type ConsTensorStartingIndex (axis :: Nat) (dims :: [Nat]) = ConsTensorStartingIndex' axis dims 0

type family ConsTensorStartingIndex' (axis :: Nat) (dims :: [Nat]) (i :: Nat) :: [Nat] where
    ConsTensorStartingIndex' _ '[]       _ = '[]
    ConsTensorStartingIndex' a (d ': ds) i =
        If (a == i) 1 0 ': ConsTensorStartingIndex' a ds (i + 1)

-- | Shape of subtensor being cons'ed to the tensor @dims@.
--
-- >>> :kind! ConsSubtensorDims 0 [2,3,4]  
-- ConsSubtensorDims 0 [2,3,4] :: [Nat] 
-- = '[1, 3, 4]                
-- 
-- >>> :kind! ConsSubtensorDims 1 [2,3,4]  
-- ConsSubtensorDims 1 [2,3,4] :: [Nat] 
-- = '[2, 1, 4]                
-- 
-- >>> :kind! ConsSubtensorDims 2 [2,3,4]  
-- ConsSubtensorDims 2 [2,3,4] :: [Nat] 
-- = '[2, 3, 1]                
type ConsSubtensorDims (axis :: Nat) (dims :: [Nat]) = ConsSubtensorDims' axis dims 0

type family ConsSubtensorDims' (axis :: Nat) (dims :: [Nat]) (i :: Nat) :: [Nat] where
    ConsSubtensorDims' _ '[]       _ = '[]
    ConsSubtensorDims' a (d ': ds) i =
        If (a == i) 1 d ': ConsSubtensorDims' a ds (i + 1)

-- | Shape of the tensor after cons'ing
--
-- >>> :kind! DimsAfterCons 0 [2,3,4]
-- DimsAfterCons 0 [2,3,4] :: [Nat]
-- = '[3, 3, 4]
--
-- >>> :kind! DimsAfterCons 1 [2,3,4]
-- DimsAfterCons 1 [2,3,4] :: [Nat]
-- = '[2, 4, 4]
--
-- >>> :kind! DimsAfterCons 2 [2,3,4]
-- DimsAfterCons 2 [2,3,4] :: [Nat]
-- = '[2, 3, 5]
type family DimsAfterCons (axis :: Nat) (dims :: [Nat]) :: [Nat] where
    DimsAfterCons 0 (d ': ds) = d + 1 ': ds
    DimsAfterCons a (d ': ds) = d ': DimsAfterCons (a - 1) ds
    DimsAfterCons _ '[]       = TypeError ('Text "DimsAfterCons: axis must be in range [0..(number of dimensions in the tensor)].")

---------------------------------------------------------------------------------------------------
-- | Append a subtensor along @axis@ to the tensor with shape @dims@
--
-- >>> snoc @0 (enumFromN @[2,3,4] 0) (enumFromStepN @[3,4] @Int (-1) (-1))
-- Tensor'3'3'4 [[[  0,   1,   2,   3]
--               ,[  4,   5,   6,   7]
--               ,[  8,   9,  10,  11]]
--              ,[[ 12,  13,  14,  15]
--               ,[ 16,  17,  18,  19]
--               ,[ 20,  21,  22,  23]]
--              ,[[ -1,  -2,  -3,  -4]
--               ,[ -5,  -6,  -7,  -8]
--               ,[ -9, -10, -11, -12]]]
--
-- >>> snoc @1 (enumFromN @[2,3,4] 0) (enumFromStepN @[2,4] @Int (-1) (-1))
-- Tensor'2'4'4 [[[  0,   1,   2,   3]
--               ,[  4,   5,   6,   7]
--               ,[  8,   9,  10,  11]
--               ,[ -1,  -2,  -3,  -4]]
--              ,[[ 12,  13,  14,  15]
--               ,[ 16,  17,  18,  19]
--               ,[ 20,  21,  22,  23]
--               ,[ -5,  -6,  -7,  -8]]]
--
-- >>> snoc @2 (enumFromN @[2,3,4] 0) (enumFromStepN @[2,3] @Int (-1) (-1))
-- Tensor'2'3'5 [[[  0,   1,   2,   3,  -1]
--               ,[  4,   5,   6,   7,  -2]
--               ,[  8,   9,  10,  11,  -3]]
--              ,[[ 12,  13,  14,  15,  -4]
--               ,[ 16,  17,  18,  19,  -5]
--               ,[ 20,  21,  22,  23,  -6]]]
snoc :: forall (axis :: Nat) (dims :: [Nat]) e.
        (Snoc axis dims e) =>
           Tensor dims e                                             -- ^ Tensor to snoc to.
        -> Tensor (NormalizeDims (SnocSubtensorDims axis dims)) e    -- ^ Subtensor to snoc.
        -> Tensor (DimsAfterSnoc axis dims) e
snoc t st =
        setSlice @(SnocSubtensorStartingIndex axis dims) @(SnocSubtensorDims axis dims) @(DimsAfterSnoc axis dims) @e t' st
    where
        t' = setSlice @(SnocTensorStartingIndex dims) @dims @(DimsAfterSnoc axis dims) z t
        z = fill @(DimsAfterSnoc axis dims) @e (head $ toList t)
{-# INLINE snoc #-}

-- | Constraints for 'snoc'.
type Snoc (axis :: Nat) (dims :: [Nat]) e =
    ( SetSlice (SnocSubtensorStartingIndex axis dims) (SnocSubtensorDims axis dims) (DimsAfterSnoc axis dims) e
    , SetSlice (SnocTensorStartingIndex dims) dims (DimsAfterSnoc axis dims) e
    , dims ~ NormalizeDims dims
    , Fill (DimsAfterSnoc axis dims) e
    )

type family SnocTensorStartingIndex (dims :: [Nat]) :: [Nat] where
    SnocTensorStartingIndex '[]       = '[]
    SnocTensorStartingIndex (_ ': ds) = 0 ': SnocTensorStartingIndex ds

type SnocSubtensorStartingIndex (axis :: Nat) (dims :: [Nat]) = SnocSubtensorStartingIndex' axis dims 0

type family SnocSubtensorStartingIndex' (axis :: Nat) (dims :: [Nat]) (i :: Nat) :: [Nat] where
    SnocSubtensorStartingIndex' _ '[]       _ = '[]
    SnocSubtensorStartingIndex' a (d ': ds) i =
        If (a == i) d 0 ': SnocSubtensorStartingIndex' a ds (i + 1)

-- | Shape of subtensor being snoc'ed to the tensor @dims@.
--
-- >>> :kind! SnocSubtensorDims 0 [2,3,4]  
-- SnocSubtensorDims 0 [2,3,4] :: [Nat] 
-- = '[1, 3, 4]
-- 
-- >>> :kind! SnocSubtensorDims 1 [2,3,4]  
-- SnocSubtensorDims 1 [2,3,4] :: [Nat] 
-- = '[2, 1, 4]
-- 
-- >>> :kind! SnocSubtensorDims 2 [2,3,4]  
-- SnocSubtensorDims 2 [2,3,4] :: [Nat] 
-- = '[2, 3, 1]
type SnocSubtensorDims (axis :: Nat) (dims :: [Nat]) = SnocSubtensorDims' axis dims 0

type family SnocSubtensorDims' (axis :: Nat) (dims :: [Nat]) (i :: Nat) :: [Nat] where
    SnocSubtensorDims' _ '[]       _ = '[]
    SnocSubtensorDims' a (d ': ds) i =
        If (a == i) 1 d ': SnocSubtensorDims' a ds (i + 1)

-- | Shape of the tensor after snoc'ing
--
-- >>> :kind! DimsAfterSnoc 0 [2,3,4]
-- DimsAfterSnoc 0 [2,3,4] :: [Nat]
-- = '[3, 3, 4]
--
-- >>> :kind! DimsAfterSnoc 1 [2,3,4]
-- DimsAfterSnoc 1 [2,3,4] :: [Nat]
-- = '[2, 4, 4]
--
-- >>> :kind! DimsAfterSnoc 2 [2,3,4]
-- DimsAfterSnoc 2 [2,3,4] :: [Nat]
-- = '[2, 3, 5]
type family DimsAfterSnoc (axis :: Nat) (dims :: [Nat]) :: [Nat] where
    DimsAfterSnoc 0 (d ': ds) = d + 1 ': ds
    DimsAfterSnoc a (d ': ds) = d ': DimsAfterSnoc (a - 1) ds
    DimsAfterSnoc _ '[]       = TypeError ('Text "DimsAfterSnoc: axis must be in range [0..(number of dimensions in the tensor)].")

---------------------------------------------------------------------------------------------------
-- | Append the second tensor @dims1@ to the first tensor @dims0@ along @axis@.
--
-- >>> append @0 (enumFromN @[2,3,4] 0) (enumFromStepN @[2,3,4] @Int (-1) (-1))
-- Tensor'4'3'4 [[[   0,   1,   2,   3]
--               ,[   4,   5,   6,   7]
--               ,[   8,   9,  10,  11]]
--              ,[[  12,  13,  14,  15]
--               ,[  16,  17,  18,  19]
--               ,[  20,  21,  22,  23]]
--              ,[[  -1,  -2,  -3,  -4]
--               ,[  -5,  -6,  -7,  -8]
--               ,[  -9, -10, -11, -12]]
--              ,[[ -13, -14, -15, -16]
--               ,[ -17, -18, -19, -20]
--               ,[ -21, -22, -23, -24]]]
--
-- >>> append @1 (enumFromN @[2,3,4] 0) (enumFromStepN @[2,3,4] @Int (-1) (-1))
-- Tensor'2'6'4 [[[   0,   1,   2,   3]
--               ,[   4,   5,   6,   7]
--               ,[   8,   9,  10,  11]
--               ,[  -1,  -2,  -3,  -4]
--               ,[  -5,  -6,  -7,  -8]
--               ,[  -9, -10, -11, -12]]
--              ,[[  12,  13,  14,  15]
--               ,[  16,  17,  18,  19]
--               ,[  20,  21,  22,  23]
--               ,[ -13, -14, -15, -16]
--               ,[ -17, -18, -19, -20]
--               ,[ -21, -22, -23, -24]]]
-- >>> append @2 (enumFromN @[2,3,4] 0) (enumFromStepN @[2,3,4] @Int (-1) (-1))
-- Tensor'2'3'8 [[[   0,   1,   2,   3,  -1,  -2,  -3,  -4]
--               ,[   4,   5,   6,   7,  -5,  -6,  -7,  -8]
--               ,[   8,   9,  10,  11,  -9, -10, -11, -12]]
--              ,[[  12,  13,  14,  15, -13, -14, -15, -16]
--               ,[  16,  17,  18,  19, -17, -18, -19, -20]
--               ,[  20,  21,  22,  23, -21, -22, -23, -24]]]
append :: forall (axis :: Nat) (dims0 :: [Nat]) (dims1 :: [Nat]) e.
    (Append axis dims0 dims1 e)
    => Tensor dims0 e       -- ^ 
    -> Tensor dims1 e       -- ^ 
    -> Tensor (DimsAfterAppend axis dims0 dims1) e
append t0 t1 =
        setSlice @(AppendSndTensorStartingIndex axis dims1) @dims1 @(DimsAfterAppend axis dims0 dims1) @e t0' t1
    where
        t0' = setSlice @(AppendFstTensorStartingIndex dims0) @dims0 @(DimsAfterAppend axis dims0 dims1) z t0
        z = fill @(DimsAfterAppend axis dims0 dims1) @e (head $ toList t0)
{-# INLINE append #-}

-- | Constraints for 'append'.
type Append (axis :: Nat) (dims0 :: [Nat]) (dims1 :: [Nat]) e =
    ( SetSlice (AppendFstTensorStartingIndex dims0) dims0 (DimsAfterAppend axis dims0 dims1) e
    , SetSlice (AppendSndTensorStartingIndex axis dims1) dims1 (DimsAfterAppend axis dims0 dims1) e
    , dims0 ~ NormalizeDims dims0
    , dims1 ~ NormalizeDims dims1
    , Fill (DimsAfterAppend axis dims0 dims1) e
    )

type family AppendFstTensorStartingIndex (dims :: [Nat]) :: [Nat] where
    AppendFstTensorStartingIndex '[]       = '[]
    AppendFstTensorStartingIndex (_ ': ds) = 0 ': SnocTensorStartingIndex ds

type AppendSndTensorStartingIndex (axis :: Nat) (dims :: [Nat]) = AppendSndTensorStartingIndex' axis dims 0

type family AppendSndTensorStartingIndex' (axis :: Nat) (dims :: [Nat]) (i :: Nat) :: [Nat] where
    AppendSndTensorStartingIndex' _ '[]       _ = '[]
    AppendSndTensorStartingIndex' a (d ': ds) i =
        If (a == i) d 0 ': AppendSndTensorStartingIndex' a ds (i + 1)

-- | Shape of the tensor after appending
--
-- >>> :kind! DimsAfterAppend 0 [2,3,4] [5,3,4]
-- DimsAfterAppend 0 [2,3,4] [5,3,4] :: [Nat]
-- = '[7, 3, 4]
--
-- >>> :kind! DimsAfterAppend 1 [2,3,4] [2,5,4]
-- DimsAfterAppend 1 [2,3,4] [2,5,4] :: [Nat]
-- = '[2, 8, 4]
--
-- >>> :kind! DimsAfterAppend 2 [2,3,4] [2,3,5]
-- DimsAfterAppend 2 [2,3,4] [2,3,5] :: [Nat]
-- = '[2, 3, 9]
type DimsAfterAppend (axis :: Nat) (dims0 :: [Nat]) (dims1 :: [Nat]) = DimsAfterAppend' axis dims0 dims1 0

type family DimsAfterAppend' (axis :: Nat) (dims0 :: [Nat]) (dims1 :: [Nat]) (i :: Nat) :: [Nat] where
    DimsAfterAppend' _ '[]         (d1 ': d1s) _ = TypeError ('Text "DimsAfterAppend: Tensors must have the same number of dimensions.")
    DimsAfterAppend' _ (d0 ': d0s) '[]         _ = TypeError ('Text "DimsAfterAppend: Tensors must have the same number of dimensions.")
    DimsAfterAppend' a '[]         '[]         a = TypeError ('Text "DimsAfterAppend: axis must be in range [0..(number of dimensions in the tensor)].")
    DimsAfterAppend' a '[]         '[]         i = '[]
    DimsAfterAppend' a (d0 ': d0s) (d1 ': d1s) a = d0 + d1 ': DimsAfterAppend' a d0s d1s (a + 1)
    DimsAfterAppend' a (d  ': d0s) (d  ': d1s) i = d ': DimsAfterAppend' a d0s d1s (i + 1)
    DimsAfterAppend' a (d0 ': d0s) (d1 ': d1s) i = TypeError ('Text "DimsAfterAppend: Tensors have incompatible dimensions.")

---------------------------------------------------------------------------------------------------
-- Traversals
---------------------------------------------------------------------------------------------------
instance (IsTensor dims a, IsTensor dims b) => Each (Tensor dims a) (Tensor dims b) a b where
    {-# INLINE each #-}
    each f t = unsafeFromList <$> traversed f (toList t)

---------------------------------------------------------------------------------------------------
-- Mono{Foldable, Traversable, Zip}
---------------------------------------------------------------------------------------------------
type instance Element (Tensor dims e) = e

instance (MonoFunctorCtx dims e) => MonoFunctor (Tensor dims e) where
    {-# INLINE omap #-}
    omap f = unsafeFromList . U.map @(ElemsNumber dims) f . toList

-- | Constraints for 'MonoFunctor' instance for @'Tensor' dims e@.
type MonoFunctorCtx (dims :: [Nat]) e =
    ( IsTensor dims e
    , U.Map (ElemsNumber dims)
    )

instance (MonoFoldableCtx dims e) => MonoFoldable (Tensor dims e) where
    {-# INLINE ofoldr #-}
    {-# INLINE ofoldMap #-}
    {-# INLINE ofoldl' #-}
    {-# INLINE ofoldr1Ex #-}
    {-# INLINE ofoldl1Ex' #-}

    ofoldr f z = U.foldr @(ElemsNumber dims) f z . toList

    ofoldMap f = U.foldMap @(ElemsNumber dims) f . toList

    ofoldl' f z = U.foldl @(ElemsNumber dims) f z . toList

    ofoldr1Ex f =  U.foldr1 @(ElemsNumber dims) f . toList

    ofoldl1Ex' f = U.foldl1 @(ElemsNumber dims) f . toList

-- | Constraints for 'MonoFoldable' instance for @'Tensor' dims e@.
type MonoFoldableCtx (dims :: [Nat]) e =
    ( IsTensor dims e
    , U.Foldr (ElemsNumber dims)
    , U.Foldl (ElemsNumber dims)
    , U.Foldr1 (ElemsNumber dims)
    , U.Foldl1 (ElemsNumber dims)
    )

instance (MonoTraversableCtx dims e) => MonoTraversable (Tensor dims e) where
    {-# INLINE otraverse #-}
    otraverse f t = unsafeFromList <$> traverse f (toList t)

-- | Constraints for 'MonoTraversable' instance for @'Tensor' dims e@.
type MonoTraversableCtx (dims :: [Nat]) e =
    ( IsTensor dims e
    , U.Map (ElemsNumber dims)
    , U.Foldr (ElemsNumber dims)
    , U.Foldl (ElemsNumber dims)
    , U.Foldr1 (ElemsNumber dims)
    , U.Foldl1 (ElemsNumber dims)
    )

instance (MonoZipCtx dims e) => MonoZip (Tensor dims e) where
    {-# INLINE ozipWith #-}
    {-# INLINE ozip #-}
    {-# INLINE ounzip #-}
    
    ozipWith f = \t1 t2 -> unsafeFromList $ U.zipWith @(ElemsNumber dims) f (toList t1) (toList t2)

    ozip t1 t2 = U.zip @(ElemsNumber dims) (toList t1) (toList t2)

    ounzip ps = 
        let (es1, es2) = U.unzip @(ElemsNumber dims) ps
            !t1 = unsafeFromList es1
            !t2 = unsafeFromList es2
        in (t1, t2)

-- | Constraints for 'MonoZip' instance for @'Tensor' dims e@.
type MonoZipCtx (dims :: [Nat]) e =
    ( IsTensor dims e
    , U.Map (ElemsNumber dims)
    , U.ZipWith (ElemsNumber dims)
    , U.Zip (ElemsNumber dims)
    , U.Unzip (ElemsNumber dims)
    )

---------------------------------------------------------------------------------------------------
-- Storable
---------------------------------------------------------------------------------------------------
instance (IsTensor dims e, Storable e, KnownNat (ElemsNumber dims)) => Storable (Tensor dims e) where
    {-# INLINE alignment #-}
    {-# INLINE sizeOf #-}
    {-# INLINE peek #-}
    {-# INLINE poke #-}

    alignment _ = alignment (undefined :: e)
    sizeOf    _ = elemsNumber @dims * offsetDiff (undefined :: e) (undefined :: e)
    
    peek p = unsafeFromList <$> mapM (\x -> peekByteOff p (x * size)) [0 .. count - 1]
        where
            size  = offsetDiff (undefined :: e) (undefined :: e)
            count = elemsNumber @dims

    poke p m = mapM_ (\(i, x) -> pokeByteOff p (size * i) x) $ zip [0 .. count - 1] $ toList m
        where
            size  = offsetDiff (undefined :: e) (undefined :: e)
            count = elemsNumber @dims

-- | Pass a pointer to the tensor's data to the IO action. The data may not be modified through the 'Ptr.
-- Pointer may not be used after action is complete.
unsafeWithTensorPtr :: (IsTensor dims e, Storable e, KnownNat (ElemsNumber dims)) => Tensor dims e -> (Ptr e -> IO a) -> IO a
unsafeWithTensorPtr t f = with t (f . castPtr)
{-# INLINE unsafeWithTensorPtr #-}

-- | Calculate number of padding bytes to be inserted between @a@ and @b@ based on size of @a@ and alignment of @b@
-- 
-- > Given:
-- > sizeOf    a = 4
-- > alignment b = 8
-- >
-- > 0 1 2 3 4 5 6 7 8 9 A B C D E F
-- > a a a a . . . . b b b b b b b b 
-- >         |<--->|
-- >            4 padding bytes
padding :: (Storable a, Storable b) => a -> b -> Int
padding a b = (alignB - sizeOf a) `mod` alignB
    where alignB = alignment b
{-# INLINE padding #-}

-- | Calculate offset difference between first byte of @a@ and first byte of @b@ based on size of @a@ and alignment of @b@
-- 
-- > Given:
-- > sizeOf    a = 4
-- > alignment b = 8
-- >
-- > 0 1 2 3 4 5 6 7 8 9 A B C D E F
-- > a a a a . . . . b b b b b b b b 
-- > |<----------->|        
-- >        8 bytes offset difference
offsetDiff :: (Storable a, Storable b) => a -> b -> Int
offsetDiff a b = sizeOf a + padding a b
{-# INLINE offsetDiff #-}