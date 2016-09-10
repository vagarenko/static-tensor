{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Tensor.Omap where

import Data.Tensor.Static
import Data.MonoTraversable
import TensorInstances ()

omap_ :: (Float -> Float) -> Tensor '[2, 3, 4] Float -> Tensor '[2, 3, 4] Float
omap_ f = omap f
