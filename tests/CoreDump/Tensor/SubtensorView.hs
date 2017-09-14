{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Tensor.SubtensorView where

import Control.Lens
import Data.Tensor.Static
import TensorInstances ()

subtensorView_ :: Tensor '[2, 3, 4] Float -> Tensor '[4] Float
subtensorView_ = view (subtensor @'[1, 1])
