{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Tensor.Cons_1 where

import Data.Tensor.Static
import TensorInstances ()

cons_1_ :: Tensor '[2, 4] Float -> Tensor '[2, 3, 4] Float -> Tensor '[2, 4, 4] Float
cons_1_ st t = cons @1 st t
