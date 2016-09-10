{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Matrix.SetRowElems where

import Data.Matrix.Static
import TensorInstances ()

setRowElems_ :: Matrix 4 4 Float -> [Float] -> Maybe (Matrix 4 4 Float)
setRowElems_ = setRowElems @1
