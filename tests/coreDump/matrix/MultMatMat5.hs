{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Matrix.MultMatMat5 where

import Data.Matrix.Static
import TensorInstances ()

multMatMat5_ :: Matrix 4 4 Float -> Matrix 4 4 Float -> Matrix 4 4 Float -> Matrix 4 4 Float -> Matrix 4 4 Float -> Matrix 4 4 Float
multMatMat5_ a b c d e = a `mult` b `mult` c `mult` d `mult` e
