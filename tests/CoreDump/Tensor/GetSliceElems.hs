{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Tensor.GetSliceElems where

import Data.Tensor.Static
import TensorInstances ()

getSliceElems_ :: Tensor '[2, 3, 4] Float -> [Float]
getSliceElems_ = getSliceElems @'[0, 0, 0] @'[2, 2, 2]
