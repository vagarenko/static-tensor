{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module CoreDump.Tensor.GenerateSing where

import Data.Kind
import Data.Singletons
import Data.Tensor.Static
import GHC.TypeLits
import TensorInstances ()

-- FIXME: Generates terrible Core possibly because of https://ghc.haskell.org/trac/ghc/ticket/14170
generateSing_ :: Tensor '[2, 3, 4] Float
generateSing_ =
    generate @'[2, 3, 4] @Float @([Nat] -> Constraint) @SingI $ \p ->
        case fromSing $ singByProxy p of
            [i, j, k] -> fromIntegral $ i * 12 + j * 4 + k
            _         -> undefined
