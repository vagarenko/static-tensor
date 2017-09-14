module MatMul.Linear where

import Linear.Matrix
import Linear.V4

mkMat :: [Float] -> M44 Float
mkMat (a:b:c:d:e:f:g:h:i:j:k:l:m:n:o:p:_) =
    V4 (V4 a b c d)
       (V4 e f g h)
       (V4 i j k l)
       (V4 m n o p)
mkMat _ = error "Not enough elements in the list."

mult :: M44 Float -> M44 Float -> M44 Float
mult = (!*!)