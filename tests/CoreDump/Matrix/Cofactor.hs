{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Matrix.Cofactor where

import Data.Matrix.Static
import TensorInstances ()

cofactor_ :: Matrix 6 6 Float -> Float
cofactor_ = cofactor @0 @0
