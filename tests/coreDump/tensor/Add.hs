{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Tensor.Add where

import Data.Tensor.Static
import TensorInstances ()

add_ :: Tensor '[2, 3, 4] Float -> Tensor '[2, 3, 4] Float -> Tensor '[2, 3, 4] Float
add_ = add
