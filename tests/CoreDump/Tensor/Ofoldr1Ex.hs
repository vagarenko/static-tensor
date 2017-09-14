{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Tensor.Ofoldr1Ex where

import Data.Tensor.Static
import Data.MonoTraversable
import TensorInstances ()

ofoldr1Ex_ :: (Float -> Float -> Float) -> Tensor '[2, 3, 4] Float -> Float
ofoldr1Ex_ f = ofoldr1Ex f
