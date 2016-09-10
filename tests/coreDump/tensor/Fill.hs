{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Tensor.Fill where

import Data.Tensor.Static
import TensorInstances ()

fill_ :: Float -> Tensor '[2, 3, 4] Float
fill_ = fill
