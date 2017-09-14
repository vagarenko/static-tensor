{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Vector.Cross where

import Data.Vector.Static
import TensorInstances ()

cross_ :: Vector 3 Float -> Vector 3 Float -> Vector 3 Float
cross_ = cross
