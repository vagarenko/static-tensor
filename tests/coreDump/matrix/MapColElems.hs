{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Matrix.MapColElems where

import Data.Matrix.Static
import TensorInstances ()

mapColElems_ :: Matrix 4 4 Float -> (Float -> Float) -> Matrix 4 4 Float
mapColElems_ = mapColElems @1
