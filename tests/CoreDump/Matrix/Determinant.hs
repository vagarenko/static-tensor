{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Matrix.Determinant where

import Data.Matrix.Static
import TensorInstances ()

determinant_ :: Matrix 5 5 Float -> Float
determinant_ = determinant