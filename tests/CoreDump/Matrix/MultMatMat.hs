{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Matrix.MultMatMat where

import Data.Matrix.Static
import TensorInstances ()

multMatMat_ :: Matrix 4 4 Float -> Matrix 4 4 Float -> Matrix 4 4 Float
multMatMat_ = mult
