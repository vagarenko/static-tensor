{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module MatMul.Vector4 where

import Data.Vector.Unboxed

type Matrix4x4f = Vector (Float, Float, Float, Float)

mkMat :: [Float] -> Matrix4x4f
mkMat (a:b:c:d:e:f:g:h:i:j:k:l:m:n:o:p:_) = fromList [(a,b,c,d), (e,f,g,h), (i,j,k,l), (m,n,o,p)]
mkMat _ = error "Not enough elements for matrix."

mult :: Matrix4x4f -> Matrix4x4f -> Matrix4x4f
mult a b = generate 4 go
    where
        go !i = (go' 0, go' 1, go' 2, go' 3)
            where
                go' !j = i0 * j0 +  i1 * j1 +  i2 * j2 +  i3 * j3
                    where
                        (!i0, !i1, !i2, !i3) = unsafeIndex a i
                        !j0 = nth j (unsafeIndex b 0)
                        !j1 = nth j (unsafeIndex b 1)
                        !j2 = nth j (unsafeIndex b 2)
                        !j3 = nth j (unsafeIndex b 3)
                        nth :: Int -> (Float, Float, Float, Float) -> Float
                        nth 0 (!x, _ , _ ,  _) = x
                        nth 1 ( _, !x, _ ,  _) = x
                        nth 2 ( _, _ , !x,  _) = x
                        nth 3 ( _, _ , _ , !x) = x
                        nth _ _            = error "Out of range."