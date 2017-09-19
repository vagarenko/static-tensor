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
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE StandaloneDeriving    #-}           
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE GADTs                 #-}

{-# OPTIONS_GHC -fno-solve-constant-dicts #-} -- See https://ghc.haskell.org/trac/ghc/ticket/13943#comment:2

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Matrix.Static
-- Copyright   :  (C) 2017 Alexey Vagarenko
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Alexey Vagarenko (vagarenko@gmail.com)
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------

module Data.Matrix.Static (
    -- * Matrix
      Matrix
    , MatrixConstructor
    , IsMatrix
    -- * Matrix construction
    , matrix
    , identity
    , Identity
    -- * Matrix elements
    -- ** Rows
    , row
    , Row
    , getRowElems
    , GetRowElems
    , setRowElems
    , SetRowElems
    , mapRowElems
    , MapRowElems
    -- ** Columns
    , col
    , Col
    , getColElems
    , GetColElems
    , setColElems
    , SetColElems
    , mapColElems
    , MapColElems
    -- * Matrix multiplication
    , MatrixMultDims
    , MatrixMult(..)
    -- * Matrix operations
    , transpose
    , Transpose
    , minorMatrix
    , MinorMatrix
    , Determinant(..)
    , minor
    , Minor
    , cofactor
    , Cofactor
    , cofactorMatrix
    , CofactorMatrix
    , adjugateMatrix
    , AdjugateMatrix
    , inverse
    , Inverse
    -- * Generating matrix instances
    , genMatrixInstance
) where

import Control.Lens             (Lens', (^.))
import Data.Kind                (Constraint)
import Data.Proxy               (Proxy(..))
import Data.Singletons          (type (~>))
import Data.Singletons.TH       (genDefunSymbols)
import Data.Tensor.Static       ( IsTensor(..), Tensor, TensorConstructor, NormalizeDims
                                , generate, Generate
                                , subtensor, SubtensorCtx, getSubtensorElems, GetSubtensorElems, setSubtensorElems, SetSubtensorElems
                                , mapSubtensorElems, MapSubtensorElems
                                , slice, Slice, getSliceElems, GetSliceElems, setSliceElems, SetSliceElems
                                , mapSliceElems, MapSliceElems
                                , withTensor
                                , NatsFromTo
                                , scale, Scale)
import Data.Tensor.Static.TH    (genTensorInstance)
import Data.Vector.Static       (Vector)
import GHC.TypeLits             (Nat, type (<=), type (<=?), type (-), type (+), TypeError, ErrorMessage(..))
import Language.Haskell.TH      (Q, Name, Dec)
import Type.List                (DemoteWith(..))

import qualified Data.List.NonEmpty as N
import qualified Data.List.Unrolled as U

---------------------------------------------------------------------------------------------------
-- | Matrix with @m@ rows, @n@ columns
type Matrix m n e = Tensor '[m, n] e

-- | Type of matrix data constructor.
type MatrixConstructor m n e = TensorConstructor '[m, n] e

-- | Matrix constraint.
type IsMatrix m n e = IsTensor '[m, n] e

---------------------------------------------------------------------------------------------------
-- | Alias for a conrete matrix data constructor.
matrix :: forall m n e. (IsMatrix m n e) => MatrixConstructor m n e
matrix = tensor @'[m, n] @e
{-# INLINE matrix #-}

-- | Identity matrix of size @m*m@
identity :: forall m e.
    ( IsMatrix m m e
    , Generate '[m, m] e ([Nat] -> Constraint) (IdentityWrk e)
    , Num e
    )
    => Matrix m m e -- ^
identity = generate @'[m, m] @e @([Nat] -> Constraint) @(IdentityWrk e) go
    where
        go :: forall (index :: [Nat]).
              (IdentityWrk e index) => 
              Proxy index -> e
        go _ = identityWrk @e @index
{-# INLINE identity #-}

-- | Constraints for 'identity' function.
type Identity m e =
    ( IsMatrix m m e
    , Generate '[m, m] e ([Nat] -> Constraint) (IdentityWrk e)
    , Num e
    )    

class IdentityWrk e (index :: [Nat]) where
    identityWrk :: e

instance {-# OVERLAPPABLE #-} (Num e) => IdentityWrk e '[i, j] where
    identityWrk = 0
    {-# INLINE identityWrk #-}

instance {-# OVERLAPPING #-} (Num e) => IdentityWrk e '[i, i] where
    identityWrk = 1
    {-# INLINE identityWrk #-}

---------------------------------------------------------------------------------------------------
-- | Lens for the row number @r@ of the matrix @m@x@n@.
--
-- >>> matrix @2 @2 @Float 0 1 2 3 ^. row @0
-- Tensor'2 [0.0,1.0]
--
-- >>> set (row @1) (vector @2 @Float 20 30) (matrix @2 @2 @Float 0 1 2 3)
-- Tensor'2'2 [[0.0,1.0],[20.0,30.0]]
row :: forall (r :: Nat) (m :: Nat) (n :: Nat) e.
    (Row r m n e)
    => Lens' (Matrix m n e) (Vector n e)    -- ^
row = subtensor @'[r] @'[m, n] @e
{-# INLINE row #-}

-- | Constraints for 'row' function.
type Row (r :: Nat) (m :: Nat) (n :: Nat) e =
    ( SubtensorCtx '[r] '[m, n] e
    , r <= m - 1                   -- TODO: Why do I need this constraint?
    , NormalizeDims '[n] ~ '[n]    -- TODO: Why do I need this constraint?
    )

-- | List of elements of the row number @r@ of the matrix @m@x@n@.
--
-- >>> getRowElems @0 (matrix @2 @2 @Float 0 1 2 3)
-- [0.0,1.0]
getRowElems :: forall (r :: Nat) (m :: Nat) (n :: Nat) e.
    (GetRowElems r m n e)
    => Matrix m n e         -- ^
    -> [e]
getRowElems = getSubtensorElems @'[r] @'[m, n] @e
{-# INLINE getRowElems #-}

-- | Constraints for 'getRowElems' function.
type GetRowElems (r :: Nat) (m :: Nat) (n :: Nat) e =
    GetSubtensorElems '[r] '[m, n] e

-- | Put elements of the list into row number @r@. The list must have enough elements.
--
-- >>> setRowElems @1 (matrix @2 @2 @Float 0 1 2 3) [20, 30]
-- Just Tensor'2'2 [[0.0,1.0],[20.0,30.0]]
--
-- >>> setRowElems @1 (matrix @2 @2 @Float 0 1 2 3) [20]
-- Nothing
setRowElems :: forall (r :: Nat) (m :: Nat) (n :: Nat) e.
    (SetRowElems r m n e)
    => Matrix m n e             -- ^ The matrix.
    -> [e]                      -- ^ New row elements.
    -> Maybe (Matrix m n e)
setRowElems = setSubtensorElems @'[r] @'[m, n] @e
{-# INLINE setRowElems #-}

-- | Constraints for 'setRowElems' function.
type SetRowElems (r :: Nat) (m :: Nat) (n :: Nat) e =
    SetSubtensorElems '[r] '[m, n] e

-- | Apply a function to all elements of the row number @r@.
--
-- >>> mapRowElems @1 (matrix @2 @2 @Float 0 1 2 3) (* 100)
-- Tensor'2'2 [[0.0,1.0],[200.0,300.0]]
mapRowElems :: forall (r :: Nat) (m :: Nat) (n :: Nat) e.
    (MapRowElems r m n e)
    => Matrix m n e         -- ^ The matrix.
    -> (e -> e)             -- ^ The mapping function.
    -> Matrix m n e
mapRowElems = mapSubtensorElems @'[r] @'[m, n] @e
{-# INLINE mapRowElems #-}

-- | Constraints for 'mapRowElems' function.
type MapRowElems (r :: Nat) (m :: Nat) (n :: Nat) e =
    MapSubtensorElems '[r] '[m, n] e

---------------------------------------------------------------------------------------------------
-- | Lens for the column number @c@ of the matrix @m@x@n@.
--
-- >>> matrix @2 @2 @Float 0 1 2 3 ^. col @0
-- Tensor'2 [0.0,2.0]
--
-- >>> set (col @1) (vector @2 @Float 10 30) (matrix @2 @2 @Float 0 1 2 3)
-- Tensor'2'2 [[0.0,10.0],[2.0,30.0]]
col :: forall (c :: Nat) (m :: Nat) (n :: Nat) e.
    (Col c m n e)
    => Lens' (Matrix m n e) (Vector m e)        -- ^
col = slice @'[0, c] @'[m, 1] @'[m, n] @e
{-# INLINE col #-}

-- | Constraints for 'col' function.
type Col (c :: Nat) (m :: Nat) (n :: Nat) e =
    ( Slice '[0, c] '[m, 1] '[m, n] e
    , NormalizeDims '[m, 1] ~ '[m]          -- TODO: Why do I need this constraint?
    )

-- | List of elements of the column number @c@ of the matrix @m@x@n@.
--
-- >>> getColElems @0 (matrix @2 @2 @Float 0 1 2 3)
-- [0.0,2.0]
getColElems :: forall (c :: Nat) (m :: Nat) (n :: Nat) e.
    (GetColElems c m n e)
    => Matrix m n e             -- ^
    -> [e]
getColElems = getSliceElems @'[0, c] @'[m, 1] @'[m, n] @e
{-# INLINE getColElems #-}

-- | Constraints for 'getColElems' function.
type GetColElems (c :: Nat) (m :: Nat) (n :: Nat) e =
    GetSliceElems '[0, c] '[m, 1] '[m, n] e

-- | Put elements of the list into column number @r@. The list must have enough elements.
--
-- >>> setColElems @1 (matrix @2 @2 @Float 0 1 2 3) [10, 30]
-- Just Tensor'2'2 [[0.0,10.0],[2.0,30.0]]
--
-- >>> setColElems @1 (matrix @2 @2 @Float 0 1 2 3) [10]
-- Nothing
setColElems :: forall (c :: Nat) (m :: Nat) (n :: Nat) e.
    (SetColElems c m n e)
    => Matrix m n e         -- ^ The matrix.
    -> [e]                  -- ^ New column elements.
    -> Maybe (Matrix m n e)
setColElems = setSliceElems @'[0, c] @'[m, 1] @'[m, n] @e
{-# INLINE setColElems #-}

-- | Constraints for 'setColElems' function.
type SetColElems (c :: Nat) (m :: Nat) (n :: Nat) e =
    SetSliceElems '[0, c] '[m, 1] '[m, n] e

-- | Apply a function to all elements of the column number @c@.
--
-- >>> mapColElems @1 (matrix @2 @2 @Float 0 1 2 3) (* 100)
-- Tensor'2'2 [[0.0,100.0],[2.0,300.0]]
mapColElems :: forall (c :: Nat) (m :: Nat) (n :: Nat) e.
    (MapColElems c m n e)
    => Matrix m n e         -- ^ 
    -> (e -> e)             -- ^ 
    -> Matrix m n e
mapColElems = mapSliceElems @'[0, c] @'[m, 1] @'[m, n] @e
{-# INLINE mapColElems #-}

-- | Constraints for 'mapColElems' function.
type MapColElems (c :: Nat) (m :: Nat) (n :: Nat) e =
    MapSliceElems '[0, c] '[m, 1] '[m, n] e

---------------------------------------------------------------------------------------------------
type family ReverseIndex (index :: [Nat]) :: [Nat] where
    ReverseIndex '[i, j] = '[j, i]

type TransposeGo m n e index = GetSliceElems (ReverseIndex index) [1, 1] [m, n] e
$(genDefunSymbols [''TransposeGo])

-- | Transpose a matrix.
transpose :: forall m n e.
    (Transpose m n e)
    => Matrix m n e         -- ^ 
    -> Matrix n m e
transpose m = generate @'[n, m] @e @([Nat] ~> Constraint) @(TransposeGoSym3 m n e) go
    where
        go :: forall (index :: [Nat]).
            (TransposeGo m n e index)
            => Proxy index -> e
        go _ = head $ getSliceElems @(ReverseIndex index) @[1, 1] m
        {-# INLINE go #-}
{-# INLINE transpose #-}

-- | Constraints for 'transpose' function.
type Transpose m n e =
    ( IsMatrix m n e
    , IsMatrix n m e
    , Generate '[n, m] e ([Nat] ~> Constraint) (TransposeGoSym3 m n e)
    )

---------------------------------------------------------------------------------------------------
-- Matrix multiplication.
---------------------------------------------------------------------------------------------------
-- | Shape of the result of matrix multiplication.
type family MatrixMultDims (dims0 :: [Nat]) (dims1 :: [Nat]) :: [Nat] where
    MatrixMultDims '[m, n] '[n, o] = '[m, o]  -- matrix m*n mult by matrix n*o makes matrix m*o
    MatrixMultDims '[n   ] '[n, o] = '[o   ]  -- vector n   mult by matrix n*o makes vector o
    MatrixMultDims '[m, n] '[n   ] = '[m   ]  -- matrix m*n mult by vector n   makes vector m
    MatrixMultDims a       b       =
        TypeError (
            'Text "Matrices of shapes "
            ':<>: 'ShowType a
            ':<>: 'Text " and "
            ':<>: 'ShowType b
            ':<>: 'Text " are incompatible for multiplication.")

-- | Matrix multiplication.
class MatrixMult (dims0 :: [Nat]) (dims1 :: [Nat]) e where
    -- | Multiply two matrices, or matrix and vector. Matrices (or matrix and vector) must have compatible dimensions.
    mult :: 
        ( IsTensor dims0 e
        , IsTensor dims1 e
        , IsTensor (MatrixMultDims dims0 dims1) e
        )
        => Tensor dims0 e                           -- ^ 
        -> Tensor dims1 e                           -- ^ 
        -> Tensor (MatrixMultDims dims0 dims1) e

-- | Get 0-th element of an index.
type family Index0 (index :: [Nat]) :: Nat where
    Index0 (i ': _) = i

-- | Get 1-st element of an index.
type family Index1 (index :: [Nat]) :: Nat where
    Index1 (_ ': j ': _ ) = j

-------------------------------------------------------------------------------
type MultMatMatGo (m :: Nat) (n :: Nat) (o :: Nat) e (index :: [Nat]) =
    ( GetRowElems (Index0 index) m n e
    , GetColElems (Index1 index) n o e
    , U.Sum n e
    , U.ZipWith n
    )
$(genDefunSymbols [''MultMatMatGo])

-- | Multiply two matrices.
instance ( Num e
         , Generate (MatrixMultDims '[m, n] '[n, o]) e ([Nat] ~> Constraint) (MultMatMatGoSym4 m n o e)
         ) =>
         MatrixMult '[m, n] '[n, o] e where
    mult m0 m1 = generate @(MatrixMultDims '[m, n] '[n, o]) @e @([Nat] ~> Constraint) @(MultMatMatGoSym4 m n o e) go
        where
            go :: forall (index :: [Nat]).
                ( GetRowElems (Index0 index) m n e
                , GetColElems (Index1 index) n o e
                , U.Sum n e
                , U.ZipWith n
                ) =>
                Proxy index -> e
            go _ = go' @(Index0 index) @(Index1 index)
            {-# INLINE go #-}

            go' :: forall (i :: Nat) (j :: Nat).
                ( GetRowElems i m n e
                , GetColElems j n o e
                , U.Sum n e
                , U.ZipWith n
                ) =>
                e
            go' = U.sum @n (U.zipWith @n (*) irow jcol)
                where
                    irow = getRowElems @i m0
                    jcol = getColElems @j m1
            {-# INLINE go' #-}
    {-# INLINE mult #-}

-------------------------------------------------------------------------------
type MultVecMatGo (m :: Nat) (n :: Nat) (o :: Nat) e (index :: [Nat]) =
    ( GetColElems (Index0 index) n o e
    , U.Sum n e
    , U.ZipWith n
    )
$(genDefunSymbols [''MultVecMatGo])

-- | Multiply vector and matrix.
instance ( Num e
         , Generate (MatrixMultDims '[n] '[n, o]) e ([Nat] ~> Constraint) (MultVecMatGoSym4 m n o e)
         ) =>
         MatrixMult '[n] '[n, o] e where
    mult v m = generate @(MatrixMultDims '[n] '[n, o]) @e @([Nat] ~> Constraint) @(MultVecMatGoSym4 m n o e) go
        where
            go :: forall (index :: [Nat]).
                ( GetColElems (Index0 index) n o e
                , U.Sum n e
                , U.ZipWith n
                ) =>
                Proxy index -> e
            go _ = go' @(Index0 index)
            {-# INLINE go #-}

            go' :: forall (c :: Nat).
                ( GetColElems c n o e
                , U.Sum n e
                , U.ZipWith n
                ) =>
                e
            go' = U.sum @n (U.zipWith @n (*) irow jcol)
                where
                    irow = toList v
                    jcol = getColElems @c m
            {-# INLINE go' #-}
    {-# INLINE mult #-}

-------------------------------------------------------------------------------
type MultMatVecGo (m :: Nat) (n :: Nat) (o :: Nat) e (index :: [Nat]) =
    ( GetRowElems (Index0 index) m n e
    , U.Sum n e
    , U.ZipWith n
    )
$(genDefunSymbols [''MultMatVecGo])

-- | Multiply matrix and vector.
instance ( Num e
         , Generate (MatrixMultDims '[m, n] '[n]) e ([Nat] ~> Constraint) (MultMatVecGoSym4 m n o e)
         ) =>
         MatrixMult '[m, n] '[n] e where
    mult m v = generate @(MatrixMultDims '[m, n] '[n]) @e @([Nat] ~> Constraint) @(MultMatVecGoSym4 m n o e) go
        where
            go :: forall (index :: [Nat]).
                ( GetRowElems (Index0 index) m n e
                , U.Sum n e
                , U.ZipWith n
                ) =>
                Proxy index -> e
            go _ = go' @(Index0 index)
            {-# INLINE go #-}

            go' :: forall (r :: Nat).
                ( GetRowElems r m n e
                , U.Sum n e
                , U.ZipWith n
                ) =>
                e
            go' = U.sum @n (U.zipWith @n (*) irow jcol)
                where
                    irow = getRowElems @r m
                    jcol = toList v
            {-# INLINE go' #-}
    {-# INLINE mult #-}

---------------------------------------------------------------------------------------------------
type family MinorMatrixNewIndex (cutIndex :: Nat) (index :: Nat) :: Nat where
    MinorMatrixNewIndex 0  i = i + 1
    MinorMatrixNewIndex ci i = MinorMatrixNewIndex' ci i (i <=? ci - 1)

--
type family MinorMatrixNewIndex' (cutIndex :: Nat) (index :: Nat) (indexLTcutIndex :: Bool) :: Nat where
    MinorMatrixNewIndex' ci i 'True  = i
    MinorMatrixNewIndex' ci i 'False = i + 1

type MinorMatrixGo (i :: Nat) (j :: Nat) (n :: Nat) e (index :: [Nat]) =
    (GetSliceElems [ (MinorMatrixNewIndex i (Index0 index))
                   , (MinorMatrixNewIndex j (Index1 index))
                   ]
                   [1, 1]
                   [n, n]
                   e
    )
$(genDefunSymbols [''MinorMatrixGo])

-- | Minor matrix is a matrix made by deleting @i@-th row and @j@-th column from given square matrix.
minorMatrix :: forall (i :: Nat) (j :: Nat) (n :: Nat) e.
    (Generate ([n - 1, n - 1]) e ([Nat] ~> Constraint) (MinorMatrixGoSym4 i j n e))
    => Matrix n n e                 -- ^ 
    -> Matrix (n - 1) (n - 1) e     -- ^ 
minorMatrix m = generate @([n - 1, n - 1]) @e @([Nat] ~> Constraint) @(MinorMatrixGoSym4 i j n e) go
    where
        go :: forall (index :: [Nat]).
            (MinorMatrixGo i j n e index) =>
            Proxy index -> e
        go _ = go' @(MinorMatrixNewIndex i (Index0 index)) @(MinorMatrixNewIndex j (Index1 index))
        {-# INLINE go #-}

        go' :: forall (r :: Nat) (c :: Nat). (GetSliceElems [r, c] [1, 1] [n, n] e) => e
        go' = head $ getSliceElems @[r, c] @[1, 1] @[n, n] @e m
        {-# INLINE go' #-}
{-# INLINE minorMatrix #-}

-- | Constraint for 'minorMatrix' function.
type MinorMatrix (i :: Nat) (j :: Nat) (n :: Nat) e =
    Generate ([n - 1, n - 1]) e ([Nat] ~> Constraint) (MinorMatrixGoSym4 i j n e)

---------------------------------------------------------------------------------------------------
-- | Determinant of a matrix.
class Determinant (n :: Nat) e where
    determinant :: (Num e) => Matrix n n e -> e

instance {-# OVERLAPPING #-}
    (Num e, IsMatrix 2 2 e)
    => Determinant 2 e
    where
    determinant m =
        withTensor m $ \a b c d -> a * d - b * c
    {-# INLINE determinant #-}

instance {-# OVERLAPPING #-}
    (Num e, IsMatrix 3 3 e)
    => Determinant 3 e
    where
    determinant m =
        withTensor m $ \a b c d e f g h i ->
            a * (e * i - f * h) - b * (d * i - f * g) + c * (d * h - e * g)
    {-# INLINE determinant #-}

-- | Sign is positive for even @n@ and negative for odd.
class Sign (n :: Nat) where
    sign :: (Num a) => a

instance {-# OVERLAPPING #-} Sign 0 where
    sign = 1
    {-# INLINE sign #-}

instance {-# OVERLAPPABLE #-} (Sign (n - 1)) => Sign n where
    sign = (-1) * sign @(n - 1)
    {-# INLINE sign #-}

type DeterminantGo (n :: Nat) e (j :: Nat) =
    ( Determinant (n - 1) e
    , GetSliceElems [0, j] [1, 1] [n, n] e
    , MinorMatrix 0 j n e
    , Sign j
    )
$(genDefunSymbols [''DeterminantGo])

instance {-# OVERLAPPABLE #-}
    ( Num e
    , IsMatrix n n e
    , DemoteWith Nat (Nat ~> Constraint) (DeterminantGoSym2 n e) (NatsFromTo 0 (n - 1))
    , U.Sum n e
    )
    => Determinant n e
    where
    determinant m = U.sum @n $ demoteWith @Nat @(Nat ~> Constraint) @(DeterminantGoSym2 n e) @(NatsFromTo 0 (n - 1)) go
        where
            go :: forall (j :: Nat).
                (DeterminantGo n e j)
                => Proxy j -> e
            go _ = sign @j * el * determinant (minorMatrix @0 @j @n @e m)
                where
                    el = head $ getSliceElems @[0, j] @[1, 1] @[n, n] @e m
            {-# INLINE go #-}
    {-# INLINE determinant #-}

---------------------------------------------------------------------------------------------------
-- | Minor is the determinant of minor matrix.
minor :: forall (i :: Nat) (j :: Nat) (n :: Nat) e.
    (Minor i j n e)
    => Matrix n n e         -- ^
    -> e
minor = determinant @(n - 1) @e . minorMatrix @i @j @n @e
{-# INLINE minor #-}

-- | Constraint for 'minor' function.
type Minor (i :: Nat) (j :: Nat) (n :: Nat) e =
    ( MinorMatrix i j n e
    , Determinant (n - 1) e
    , Num e
    )

---------------------------------------------------------------------------------------------------
-- | @'cofactor' \@i \@j@ is the @'minor' \@i \@j@ multiplied by @(-1) ^ (i + j)@.
cofactor :: forall (i :: Nat) (j :: Nat) (n :: Nat) e.
    (Cofactor i j n e)
    => Matrix n n e         -- ^
    -> e
cofactor m = sign @(i + j) * minor @i @j @n @e m
{-# INLINE cofactor #-}

-- | Constraint for 'cofactor' function.
type Cofactor (i :: Nat) (j :: Nat) (n :: Nat) e =
    ( Minor i j n e
    , Sign (i + j)
    )

---------------------------------------------------------------------------------------------------
type CofactorMatrixGo (n :: Nat) e (index :: [Nat]) =
    (Cofactor (Index0 index) (Index1 index) n e)
$(genDefunSymbols [''CofactorMatrixGo])

-- | The matrix formed by all of the cofactors of given square matrix.
cofactorMatrix :: forall (n :: Nat) e.
    (CofactorMatrix n e)
    => Matrix n n e         -- ^
    -> Matrix n n e
cofactorMatrix m = generate @([n, n]) @e @([Nat] ~> Constraint) @(CofactorMatrixGoSym2 n e) go
    where
        go :: forall (index :: [Nat]).
            (Cofactor (Index0 index) (Index1 index) n e) =>
            Proxy index -> e
        go _ = go' @(Index0 index) @(Index1 index)
        {-# INLINE go #-}

        go' :: forall (i :: Nat) (j :: Nat).
            (Cofactor i j n e) => e
        go' = cofactor @i @j @n @e m
        {-# INLINE go' #-}
{-# INLINE cofactorMatrix #-}

-- | Constraint for 'cofactorMatrix' function.
type CofactorMatrix (n :: Nat) e =
    Generate [n, n] e ([Nat] ~> Constraint) (CofactorMatrixGoSym2 n e)

---------------------------------------------------------------------------------------------------
-- | Adjugate matrix of given square matrix is the transpose of its cofactor matrix.
--
-- @adjugateMatrix = transpose . cofactorMatrix@
--
adjugateMatrix :: forall (n :: Nat) e.
    (AdjugateMatrix n e)
    => Matrix n n e         -- ^
    -> Matrix n n e
adjugateMatrix = transpose . cofactorMatrix
{-# INLINE adjugateMatrix #-}

-- | Constraint for 'adjugateMatrix' function.
type AdjugateMatrix (n :: Nat) e =
    (CofactorMatrix n e, Transpose n n e)

---------------------------------------------------------------------------------------------------
-- | Inverse of the matrix.
inverse :: forall (n :: Nat) e.
    (Inverse n e)
    => Matrix n n e         -- ^
    -> Matrix n n e
inverse m = scale (adjugateMatrix m) (1 / determinant m)
{-# INLINE inverse #-}

-- | Constraint for 'inverse' function.
type Inverse (n :: Nat) e =
    (AdjugateMatrix n e, Determinant n e, Fractional e, Scale '[n, n] e)

---------------------------------------------------------------------------------------------------
-- | Generate instance of a matrix.
genMatrixInstance :: Int       -- ^ Number of rows.
                  -> Int       -- ^ Number of columns.
                  -> Name      -- ^ Type of elements.
                  -> Q [Dec]
genMatrixInstance m n elemTypeName = genTensorInstance (N.fromList [m, n]) elemTypeName
