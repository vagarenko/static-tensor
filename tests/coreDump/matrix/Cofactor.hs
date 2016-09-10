{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Matrix.Cofactor where

import Data.Matrix.Static
import TensorInstances ()

-- FIXME: Generates terrible Core.
cofactor_ :: Matrix 5 5 Float -> Float
cofactor_ = cofactor @0 @0
