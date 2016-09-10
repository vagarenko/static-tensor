{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Matrix.Minor where

import Data.Matrix.Static
import TensorInstances ()

-- FIXME: Generates terrible Core.
minor_ :: Matrix 5 5 Float -> Float
minor_ = minor @0 @0
