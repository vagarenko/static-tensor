{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Tensor.Zero where

import Data.Tensor.Static
import TensorInstances ()

zero_ :: Tensor '[2, 3, 4] Float
zero_ = zero
