{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Matrix.MultMatVec where

import Data.Matrix.Static
import Data.Vector.Static
import TensorInstances ()

multMatVec_ :: Matrix 4 4 Float -> Vector 4 Float -> Vector 4 Float
multMatVec_ = mult
