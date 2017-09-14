{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Tensor.Snoc_1 where

import Data.Tensor.Static
import TensorInstances ()

snoc_1_ :: Tensor '[2, 3, 4] Float -> Tensor '[2, 4] Float -> Tensor '[2, 4, 4] Float
snoc_1_ st t = snoc @1 st t
