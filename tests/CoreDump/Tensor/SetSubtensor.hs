{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Tensor.SetSubtensor where

import Data.Tensor.Static
import TensorInstances ()

setSubtensor_ :: Tensor '[2, 3, 4] Float -> Subtensor '[0] '[2, 3, 4] Float -> Tensor '[2, 3, 4] Float
setSubtensor_ = setSubtensor @'[0]
