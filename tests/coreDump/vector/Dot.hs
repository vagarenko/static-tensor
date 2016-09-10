{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Vector.Dot where

import Data.Vector.Static
import TensorInstances ()

dot_ :: Vector 4 Float -> Vector 4 Float -> Float
dot_ = dot
