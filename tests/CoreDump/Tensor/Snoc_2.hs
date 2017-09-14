{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Tensor.Snoc_2 where

import Data.Tensor.Static
import TensorInstances ()

snoc_2_ :: Tensor '[2, 3, 4] Float -> Tensor '[2, 3] Float -> Tensor '[2, 3, 5] Float
snoc_2_ st t = snoc @2 st t
