{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Tensor.OfoldMap where

import Data.Tensor.Static
import Data.MonoTraversable
import TensorInstances ()

ofoldMap_ :: (Monoid w) => (Float -> w) -> Tensor '[2, 3, 4] Float -> w
ofoldMap_ f = ofoldMap f
