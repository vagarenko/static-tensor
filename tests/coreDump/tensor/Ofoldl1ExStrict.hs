{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Tensor.Ofoldl1ExStrict where

import Data.Tensor.Static
import Data.MonoTraversable
import TensorInstances ()

ofoldl1ExStrict_ :: (Float -> Float -> Float) -> Tensor '[2, 3, 4] Float -> Float
ofoldl1ExStrict_ f = ofoldl1Ex' f
