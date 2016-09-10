{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Matrix.GetRowElems where

import Data.Matrix.Static
import TensorInstances ()

getRowElems_ :: Matrix 4 4 Float -> [Float]
getRowElems_ = getRowElems @3
