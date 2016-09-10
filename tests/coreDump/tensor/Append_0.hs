{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Tensor.Append_0 where

import Data.Tensor.Static
import TensorInstances ()

append_0_ :: Tensor '[2, 3, 4] Float -> Tensor '[2, 3, 4] Float -> Tensor '[4, 3, 4] Float
append_0_ st t = append @0 st t
