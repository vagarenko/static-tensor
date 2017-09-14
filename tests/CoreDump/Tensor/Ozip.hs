{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Tensor.Ozip where

import Data.Tensor.Static
import Data.Containers
import TensorInstances ()

ozip_ :: Tensor '[2, 3, 4] Float
      -> Tensor '[2, 3, 4] Float
      -> [(Float, Float)]
ozip_ a b = ozip a b
