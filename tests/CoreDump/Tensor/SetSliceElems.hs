{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Tensor.SetSliceElems where

import Data.Tensor.Static
import TensorInstances ()

setSliceElems_ :: Tensor '[2, 3, 4] Float -> [Float] -> Maybe (Tensor '[2, 3, 4] Float)
setSliceElems_ = setSliceElems @'[0, 1, 2] @'[2, 2, 2]
