{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Tensor.EnumFromStepN where

import Data.Tensor.Static
import TensorInstances ()

enumFromStepN_ :: Float -> Float -> Tensor '[2, 3, 4] Float
enumFromStepN_ = enumFromStepN
