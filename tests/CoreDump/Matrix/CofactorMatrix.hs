{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Matrix.CofactorMatrix where

import Data.Matrix.Static
import TensorInstances ()

cofactorMatrix_ :: Matrix 4 4 Float -> Matrix 4 4 Float
cofactorMatrix_ = cofactorMatrix
