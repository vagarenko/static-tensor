{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Tensor.Ounzip where

import Data.Tensor.Static
import Data.Containers
import TensorInstances ()

ounzip_ :: [(Float, Float)]
        -> (Tensor '[2, 3, 4] Float, Tensor '[2, 3, 4] Float)
ounzip_ xs = ounzip xs
