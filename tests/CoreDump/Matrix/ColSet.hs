{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Matrix.ColSet where

import Control.Lens
import Data.Matrix.Static
import Data.Vector.Static
import TensorInstances ()

colSet_ :: Vector 3 Float -> Matrix 3 4 Float -> Matrix 3 4 Float
colSet_ v = set (col @1) v
