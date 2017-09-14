{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Tensor.Cons_0 where

import Data.Tensor.Static
import TensorInstances ()

cons_0_ :: Tensor '[3, 4] Float -> Tensor '[2, 3, 4] Float -> Tensor '[3, 3, 4] Float
cons_0_ st t = cons @0 st t
