{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Tensor.Remove_1_0 where

import Data.Tensor.Static
import TensorInstances ()

remove_1_0_ :: Tensor '[2, 3, 4] Float -> Tensor '[2, 2, 4] Float
remove_1_0_ = remove @1 @0
