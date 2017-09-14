{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module MatMul.Unrolled where

import Control.DeepSeq

data Vector4f = Vector4f
    {-# UNPACK #-} !Float
    {-# UNPACK #-} !Float
    {-# UNPACK #-} !Float
    {-# UNPACK #-} !Float
    deriving (Show, Eq)

data Matrix4x4f = Matrix4x4f
    {-# UNPACK #-} !Vector4f
    {-# UNPACK #-} !Vector4f
    {-# UNPACK #-} !Vector4f
    {-# UNPACK #-} !Vector4f             
    deriving (Show, Eq)

instance NFData Matrix4x4f where
    rnf (Matrix4x4f {}) = ()
    {-# INLINE rnf #-}


mkMat :: [Float] -> Matrix4x4f
mkMat (a:b:c:d:e:f:g:h:i:j:k:l:m:n:o:p:_) = 
    Matrix4x4f
        (Vector4f a b c d)
        (Vector4f e f g h)
        (Vector4f i j k l)
        (Vector4f m n o p)
mkMat _ = error "Not enough elements for matrix."

mult :: Matrix4x4f -> Matrix4x4f -> Matrix4x4f
mult
    (Matrix4x4f
        (Vector4f a0 a1 a2 a3)
        (Vector4f b0 b1 b2 b3)
        (Vector4f c0 c1 c2 c3)
        (Vector4f d0 d1 d2 d3))
    (Matrix4x4f
        (Vector4f x0 x1 x2 x3)
        (Vector4f y0 y1 y2 y3)
        (Vector4f z0 z1 z2 z3)
        (Vector4f w0 w1 w2 w3)) =
    Matrix4x4f
        (Vector4f (a0*x0+a1*y0+a2*z0+a3*w0) (a0*x1+a1*y1+a2*z1+a3*w1) (a0*x2+a1*y2+a2*z2+a3*w2) (a0*x3+a1*y3+a2*z3+a3*w3))
        (Vector4f (b0*x0+b1*y0+b2*z0+b3*w0) (b0*x1+b1*y1+b2*z1+b3*w1) (b0*x2+b1*y2+b2*z2+b3*w2) (b0*x3+b1*y3+b2*z3+b3*w3))
        (Vector4f (c0*x0+c1*y0+c2*z0+c3*w0) (c0*x1+c1*y1+c2*z1+c3*w1) (c0*x2+c1*y2+c2*z2+c3*w2) (c0*x3+c1*y3+c2*z3+c3*w3))
        (Vector4f (d0*x0+d1*y0+d2*z0+d3*w0) (d0*x1+d1*y1+d2*z1+d3*w1) (d0*x2+d1*y2+d2*z2+d3*w2) (d0*x3+d1*y3+d2*z3+d3*w3))