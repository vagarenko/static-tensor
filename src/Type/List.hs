{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE TypeSynonymInstances    #-}           
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE KindSignatures          #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE FlexibleContexts        #-}           
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE PolyKinds               #-}
{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE TypeFamilyDependencies  #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE InstanceSigs            #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Type.List
-- Copyright   :  (C) 2017 Alexey Vagarenko
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Alexey Vagarenko (vagarenko@gmail.com)
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------

module Type.List where

import Data.Proxy
import Data.Kind
import Data.Singletons
import GHC.TypeLits

-- | Demote type-level list of 'Nat'.
class KnownNats (ns :: [Nat]) where
    natsVal :: [Int]

instance KnownNats '[] where
    natsVal = []
    {-# INLINE natsVal #-}

instance (KnownNat n, KnownNats ns) => KnownNats (n ': ns) where
    natsVal = fromInteger (natVal (Proxy @n)) : natsVal @ns
    {-# INLINE natsVal #-}

---------------------------------------------------------------------------------------------------
-- | Make a constraint for type @x :: kx@ from 'TyFun', or partially applied constraint, or make an empty constraint.
type MkCtx :: forall (kx :: Type) -> forall (kctx :: Type) -> kctx -> kx -> Constraint
type family MkCtx kx kctx ctx x where
    MkCtx kx (kx ~> Constraint) ctx  x = Apply ctx x
    MkCtx kx (kx -> Constraint) ctx  x = ctx x
    MkCtx _  Constraint         ()   _ = ()
    MkCtx _  Type               ()   _ = ()

-- | Demote a type-level list to value-level list with a type-indexed function.
--   The function takes list element as type parameter @x@ and applies constraints @ctx@ for that element.
class DemoteWith (kx :: Type) (kctx :: Type) (ctx :: kctx) (xs :: [kx]) where
    demoteWith :: (forall (x :: kx). (MkCtx kx kctx ctx x) => Proxy x -> a) -> [a]

instance DemoteWith kx kctx ctxs '[] where
    demoteWith _ = []
    {-# INLINE demoteWith #-}

instance (DemoteWith kx kctx ctx xs, MkCtx kx kctx ctx x) => DemoteWith kx kctx ctx (x ': xs) where
    demoteWith f = f (Proxy @x) : demoteWith @kx @kctx @ctx @xs f
    {-# INLINE demoteWith #-}
