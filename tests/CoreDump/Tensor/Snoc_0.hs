{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Tensor.Snoc_0 where

import Data.Tensor.Static
import TensorInstances ()

snoc_0_ :: Tensor '[2, 3, 4] Float -> Tensor '[3, 4] Float -> Tensor '[3, 3, 4] Float
snoc_0_ st t = snoc @0 st t
