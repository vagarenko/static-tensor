{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Tensor.SetSlice where

import Data.Tensor.Static
import TensorInstances ()

setSlice_ :: Tensor '[2, 3, 4] Float -> Tensor '[2, 2, 2] Float -> Tensor '[2, 3, 4] Float
setSlice_ = setSlice @'[0, 1, 2] @'[2, 2, 2]
