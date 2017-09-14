{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Tensor.GetSubtensor where

import Data.Tensor.Static
import TensorInstances ()

getSubtensor_ :: Tensor '[2, 3, 4] Float -> Tensor '[3, 4] Float
getSubtensor_ = getSubtensor @'[0]
