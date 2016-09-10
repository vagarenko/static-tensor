{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Matrix.AdjugateMatrix where

import Data.Matrix.Static
import TensorInstances ()

-- FIXME: Generates terrible Core.
adjugateMatrix_ :: Matrix 4 4 Float -> Matrix 4 4 Float
adjugateMatrix_ = adjugateMatrix
