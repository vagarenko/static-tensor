{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Matrix.RowOver where

import Control.Lens
import Data.Matrix.Static
import Data.Vector.Static
import TensorInstances ()

rowOver_ :: (Vector 3 Float -> Vector 3 Float) -> Matrix 4 3 Float -> Matrix 4 3 Float
rowOver_ f = over (row @2) f
