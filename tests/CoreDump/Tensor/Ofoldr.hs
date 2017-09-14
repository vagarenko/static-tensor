{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Tensor.Ofoldr where

import Data.Tensor.Static
import Data.MonoTraversable
import TensorInstances ()

ofoldr_ :: (Float -> b -> b) -> b -> Tensor '[2, 3, 4] Float -> b
ofoldr_ f z = ofoldr f z
