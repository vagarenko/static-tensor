{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Tensor.TensorElemView where

import Control.Lens
import Data.Tensor.Static
import TensorInstances ()

tensorElemView_ :: Tensor '[2, 3, 4] Float -> Float
tensorElemView_ = view (tensorElem @'[1, 1, 1])
