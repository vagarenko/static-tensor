{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Matrix.GetColElems where

import Data.Matrix.Static
import TensorInstances ()

getColElems_ :: Matrix 4 4 Float -> [Float]
getColElems_ = getColElems @3
