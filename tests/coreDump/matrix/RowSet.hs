{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Matrix.RowSet where

import Control.Lens
import Data.Matrix.Static
import Data.Vector.Static
import TensorInstances ()

rowSet_ :: Vector 4 Float -> Matrix 4 4 Float -> Matrix 4 4 Float
rowSet_ v = set (row @1) v
