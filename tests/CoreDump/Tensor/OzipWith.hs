{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Tensor.OzipWith where

import Data.Tensor.Static
import Data.Containers
import TensorInstances ()

ozipWith_ :: (Float -> Float -> Float) 
          -> Tensor '[2, 3, 4] Float
          -> Tensor '[2, 3, 4] Float
          -> Tensor '[2, 3, 4] Float
ozipWith_ f = ozipWith f
