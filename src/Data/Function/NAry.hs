{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeInType              #-}
{-# LANGUAGE AllowAmbiguousTypes     #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Function.NAry
-- Copyright   :  (C) 2017 Alexey Vagarenko
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Alexey Vagarenko (vagarenko@gmail.com)
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------

module Data.Function.NAry (
      NAry
    , ApplyNAry(..)
) where

import Data.Kind                    (Type)
import Data.Proxy                   (Proxy(..))
import GHC.TypeLits                 (Nat, type (-), natVal, KnownNat)

---------------------------------------------------------------------------------------------------
-- | N-ary function from @n@ arguments of type @t@ to value of type @r@.
type family NAry (n :: Nat) (t :: Type) (r :: Type) :: Type where
    NAry 0 t r = r
    NAry n t r = t -> (NAry (n - 1) t r)

-- | Apply list of params to N-ary function.
class ApplyNAry (n :: Nat) (t :: Type) (r :: Type) where
    applyNAry :: NAry n t r -> [t] -> r

instance {-# OVERLAPPING #-} ApplyNAry 0 t r where
    applyNAry r _ = r
    {-# INLINE applyNAry #-}

instance {-# OVERLAPPABLE #-}
    (ApplyNAry (n - 1) t r, NAry n t r ~ (t -> NAry (n - 1) t r), KnownNat n)
    => ApplyNAry n t r where
    applyNAry f (x : xs) = applyNAry @(n - 1) @t @r (f x) xs
    applyNAry _ []       = error $ "Not enough params to apply to " ++ show (natVal (Proxy @n)) ++ "-ary function."
    {-# INLINE applyNAry #-}
