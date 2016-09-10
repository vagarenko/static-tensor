{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Tensor.Append_2 where

import Data.Tensor.Static
import TensorInstances ()

append_2_ :: Tensor '[2, 3, 4] Float -> Tensor '[2, 3, 4] Float -> Tensor '[2, 3, 8] Float
append_2_ st t = append @2 st t
