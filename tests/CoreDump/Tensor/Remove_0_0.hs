{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Tensor.Remove_0_0 where

import Data.Tensor.Static
import TensorInstances ()

remove_0_0_ :: Tensor '[2, 3, 4] Float -> Tensor '[1, 3, 4] Float
remove_0_0_ = remove @0 @0
