{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Matrix.RowView where

import Control.Lens
import Data.Matrix.Static
import Data.Vector.Static
import TensorInstances ()

rowView_ :: Matrix 4 4 Float -> Vector 4 Float
rowView_ = view (row @0)
