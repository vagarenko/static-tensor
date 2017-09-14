{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Tensor.TensorElemSet where

import Control.Lens
import Data.Tensor.Static
import TensorInstances ()

tensorElemSet_ :: Float -> Tensor '[2, 3, 4] Float -> Tensor '[2, 3, 4] Float
tensorElemSet_ e = set (tensorElem @'[1, 1, 1]) e      -- Had to provide 'e' arg here because 'set' is inlined only with 2 or more args.

