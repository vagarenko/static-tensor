{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Matrix.ColOver where

import Control.Lens
import Data.Matrix.Static
import Data.Vector.Static
import TensorInstances ()

colOver_ :: (Vector 4 Float -> Vector 4 Float) -> Matrix 4 4 Float -> Matrix 4 4 Float
colOver_ f = over (col @2) f
