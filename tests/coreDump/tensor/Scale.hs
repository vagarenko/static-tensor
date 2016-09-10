{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Tensor.Scale where

import Data.Tensor.Static
import TensorInstances ()

scale_ :: Tensor '[2, 3, 4] Float -> Float -> Tensor '[2, 3, 4] Float
scale_ = scale
