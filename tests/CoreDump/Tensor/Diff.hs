{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Tensor.Diff where

import Data.Tensor.Static
import TensorInstances ()

diff_ :: Tensor '[2, 3, 4] Float -> Tensor '[2, 3, 4] Float -> Tensor '[2, 3, 4] Float
diff_ = diff
