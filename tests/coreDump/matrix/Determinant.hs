{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Matrix.Determinant where

import Data.Matrix.Static
import TensorInstances ()

-- FIXME: Generates terrible Core.
determinant_ :: Matrix 4 4 Float -> Float
determinant_ = determinant