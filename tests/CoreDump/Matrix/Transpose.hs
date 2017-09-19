{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Matrix.Transpose where

import Data.Matrix.Static
import TensorInstances ()

transpose_ :: Matrix 4 3 Float -> Matrix 3 4 Float
transpose_ = transpose
