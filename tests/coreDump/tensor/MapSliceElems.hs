{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Tensor.MapSliceElems where

import Data.Tensor.Static
import TensorInstances ()

mapSliceElems_ :: Tensor '[2, 3, 4] Float -> (Float -> Float) -> Tensor '[2, 3, 4] Float
mapSliceElems_ = mapSliceElems @'[0, 1, 2] @'[2, 2, 2]
