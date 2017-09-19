{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Matrix.Minor where

import Data.Matrix.Static
import TensorInstances ()

minor_ :: Matrix 6 6 Float -> Float
minor_ = minor @0 @0
