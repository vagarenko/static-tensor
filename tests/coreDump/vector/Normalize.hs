{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Vector.Normalize where

import Data.Vector.Static
import TensorInstances ()

normalize_ :: Vector 4 Float -> NormalizedVector 4 Float
normalize_ = normalize
