{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Matrix.MultVecMat where

import Data.Matrix.Static
import Data.Vector.Static
import TensorInstances ()

multVecMat_ :: Vector 4 Float -> Matrix 4 4 Float -> Vector 4 Float
multVecMat_ = mult
