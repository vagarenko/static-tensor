{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Matrix.Inverse where

import Data.Matrix.Static
import TensorInstances ()

-- FIXME: Generates terrible Core.
inverse_ :: Matrix 4 4 Float -> Matrix 4 4 Float
inverse_ = inverse
