{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Tensor.SetSubtensorElems where

import Data.Tensor.Static
import TensorInstances ()

setSubtensorElems_ :: Tensor '[2, 3, 4] Float -> [Float] -> Maybe (Tensor '[2, 3, 4] Float)
setSubtensorElems_ = setSubtensorElems @'[0]
