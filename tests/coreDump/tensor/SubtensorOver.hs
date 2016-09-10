{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Tensor.SubtensorOver where

import Control.Lens
import Data.Tensor.Static
import TensorInstances ()

subtensorOver_ ::
       (Subtensor '[0, 1] '[2, 3, 4] Float -> Subtensor '[0, 1] '[2, 3, 4] Float)
    -> Tensor '[2, 3, 4] Float
    -> Tensor '[2, 3, 4] Float
subtensorOver_ f = over (subtensor @'[0, 1] @'[2, 3, 4]) f      -- Had to provide 'f' arg here because 'over' is inlined only with 2 or more args.
