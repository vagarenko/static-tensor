{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Matrix.SetColElems where

import Data.Matrix.Static
import TensorInstances ()

setColElems_ :: Matrix 4 4 Float -> [Float] -> Maybe (Matrix 4 4 Float)
setColElems_ = setColElems @1
