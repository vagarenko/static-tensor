{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Tensor.Cons_2 where

import Data.Tensor.Static
import TensorInstances ()

cons_2_ :: Tensor '[2, 3] Float -> Tensor '[2, 3, 4] Float -> Tensor '[2, 3, 5] Float
cons_2_ st t = cons @2 st t
