{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Tensor.GetSlice where

import Data.Tensor.Static
import TensorInstances ()

getSlice_ :: Tensor '[2, 3, 4] Float -> Tensor '[2, 2, 2] Float
getSlice_ = getSlice @'[0, 0, 0] @'[2, 2, 2]
