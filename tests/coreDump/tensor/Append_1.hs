{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Tensor.Append_1 where

import Data.Tensor.Static
import TensorInstances ()

append_1_ :: Tensor '[2, 3, 4] Float -> Tensor '[2, 3, 4] Float -> Tensor '[2, 6, 4] Float
append_1_ st t = append @1 st t
