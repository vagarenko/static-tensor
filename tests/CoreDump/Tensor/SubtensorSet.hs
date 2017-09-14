{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Tensor.SubtensorSet where

import Control.Lens
import Data.Tensor.Static
import TensorInstances ()

subtensorSet_ :: Subtensor '[1, 2] '[2, 3, 4] Float -> Tensor '[2, 3, 4] Float -> Tensor '[2, 3, 4] Float
subtensorSet_ st = set (subtensor @'[1, 2] @'[2, 3, 4]) st      -- Had to provide 'st' arg here because 'set' is inlined only with 2 or more args.
