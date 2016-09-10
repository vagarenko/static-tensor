{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Vector.VectorLen where

import Data.Vector.Static
import TensorInstances ()

vectorLen_ :: Vector 4 Float -> Float
vectorLen_ = vectorLen
