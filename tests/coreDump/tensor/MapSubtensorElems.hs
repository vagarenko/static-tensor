{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Tensor.MapSubtensorElems where

import Data.Tensor.Static
import TensorInstances ()

mapSubtensorElems_ :: Tensor '[2, 3, 4] Float -> (Float -> Float) -> Tensor '[2, 3, 4] Float
mapSubtensorElems_ = mapSubtensorElems @'[0]
