{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Tensor.Remove_2_0 where

import Data.Tensor.Static
import TensorInstances ()

remove_2_0_ :: Tensor '[2, 3, 4] Float -> Tensor '[2, 3, 3] Float
remove_2_0_ = remove @2 @0
