{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Matrix.MinorMatrix where

import Data.Matrix.Static
import TensorInstances ()

-- FIXME: Generates terrible Core.
minorMatrix_ :: Matrix 4 4 Float -> Matrix 3 3 Float
minorMatrix_ = minorMatrix @0 @0
