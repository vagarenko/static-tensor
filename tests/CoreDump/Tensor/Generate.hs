{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE RankNTypes            #-}

module CoreDump.Tensor.Generate where

import Data.Proxy
import Data.Tensor.Static
import GHC.TypeLits
import TensorInstances ()

generate_ :: (forall (index :: [Nat]). Proxy index -> Float) -> Tensor '[2, 3, 4] Float
generate_ = generate @'[2, 3, 4] @Float @_ @()
