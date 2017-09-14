{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE BangPatterns #-}

module MatMul.Vector where

import Data.Vector.Unboxed

type Matrix4x4f = Vector Float

mkMat :: [Float] -> Matrix4x4f
mkMat xs@(_a:_b:_c:_d:_e:_f:_g:_h:_i:_j:_k:_l:_m:_n:_o:_p:_) = fromList xs
mkMat _ = error "Not enough elements for matrix."

mult :: Matrix4x4f -> Matrix4x4f -> Matrix4x4f
mult a b = generate 16 go
    where
        go n = i0 * j0 +  i1 * j1 +  i2 * j2 +  i3 * j3
            where
                (!i, !j) = n `divMod` 4
                i0 = unsafeIndex a (0 + i * 4)
                i1 = unsafeIndex a (1 + i * 4)
                i2 = unsafeIndex a (2 + i * 4)
                i3 = unsafeIndex a (3 + i * 4)
                j0 = unsafeIndex b (j + 0 * 4)
                j1 = unsafeIndex b (j + 1 * 4)
                j2 = unsafeIndex b (j + 2 * 4)
                j3 = unsafeIndex b (j + 3 * 4)