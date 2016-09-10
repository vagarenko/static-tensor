{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Tensor.OfoldlStrict where

import Data.Tensor.Static
import Data.MonoTraversable
import TensorInstances ()

ofoldlStrict_ :: (a -> Float -> a) -> a -> Tensor '[2, 3, 4] Float -> a
ofoldlStrict_ f z = ofoldl' f z
