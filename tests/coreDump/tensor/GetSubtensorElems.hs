{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Tensor.GetSubtensorElems where

import Data.Tensor.Static
import TensorInstances ()

getSubtensorElems_ :: Tensor '[2, 3, 4] Float -> [Float]
getSubtensorElems_ = getSubtensorElems @'[0]
