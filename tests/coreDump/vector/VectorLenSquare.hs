{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Vector.VectorLenSquare where

import Data.Vector.Static
import TensorInstances ()

vectorLenSquare_ :: Vector 4 Float -> Float
vectorLenSquare_ = vectorLenSquare
