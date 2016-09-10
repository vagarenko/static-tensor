{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Matrix.Identity where

import Data.Matrix.Static
import TensorInstances ()

identity_ :: Matrix 4 4 Float
identity_ = identity
