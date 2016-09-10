{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module CoreDump.Tensor.GenerateKnownNats where

import Data.Kind
import Data.Singletons
import Data.Tensor.Static
import GHC.TypeLits
import TensorInstances ()
import Type.List

-- FIXME: Generates terrible Core possibly because of https://ghc.haskell.org/trac/ghc/ticket/14170
generateKnownNats_ :: Tensor '[2, 3, 4] Float
generateKnownNats_ =
    generate @'[2, 3, 4] @Float @([Nat] -> Constraint) @KnownNats $ \(_ :: Proxy index) ->
        case natsVal @index of
            [i, j, k] -> fromIntegral $ i * 12 + j * 4 + k
            _         -> undefined
