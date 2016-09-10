{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Tensor.TensorElemOver where

import Control.Lens
import Data.Tensor.Static
import TensorInstances ()

tensorElemOver_ :: (Float -> Float) -> Tensor '[2, 3, 4] Float -> Tensor '[2, 3, 4] Float
tensorElemOver_ f = over (tensorElem @'[1, 1, 1]) f      -- Had to provide 'f' arg here because 'over' is inlined only with 2 or more args.
