{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Tensor.EnumFromN where

import Data.Tensor.Static
import TensorInstances ()

enumFromN_ :: Float -> Tensor '[2, 3, 4] Float
enumFromN_ = enumFromN
