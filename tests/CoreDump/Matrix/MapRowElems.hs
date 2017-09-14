{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Matrix.MapRowElems where

import Data.Matrix.Static
import TensorInstances ()

mapRowElems_ :: Matrix 4 4 Float -> (Float -> Float) -> Matrix 4 4 Float
mapRowElems_ = mapRowElems @1
