{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Matrix.ColView where

import Control.Lens
import Data.Matrix.Static
import Data.Vector.Static
import TensorInstances ()

colView_ :: Matrix 4 4 Float -> Vector 4 Float
colView_ = view (col @0)
