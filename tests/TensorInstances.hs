{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE GADTs                 #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

module TensorInstances where

import Data.Tensor.Static.TH
import qualified Data.List.NonEmpty as N

$(genTensorInstance (N.fromList [3])       ''Float)
$(genTensorInstance (N.fromList [4])       ''Float)
$(genTensorInstance (N.fromList [2, 3])    ''Float)
$(genTensorInstance (N.fromList [2, 4])    ''Float)
$(genTensorInstance (N.fromList [3, 3])    ''Float)
$(genTensorInstance (N.fromList [3, 4])    ''Float)
$(genTensorInstance (N.fromList [4, 3])    ''Float)
$(genTensorInstance (N.fromList [4, 4])    ''Float)
$(genTensorInstance (N.fromList [5, 5])    ''Float)
$(genTensorInstance (N.fromList [6, 6])    ''Float)
$(genTensorInstance (N.fromList [1, 3, 4]) ''Float)
$(genTensorInstance (N.fromList [2, 2, 2]) ''Float)
$(genTensorInstance (N.fromList [2, 2, 4]) ''Float)
$(genTensorInstance (N.fromList [2, 3, 3]) ''Float)
$(genTensorInstance (N.fromList [2, 3, 4]) ''Float)
$(genTensorInstance (N.fromList [2, 3, 5]) ''Float)
$(genTensorInstance (N.fromList [2, 4, 4]) ''Float)
$(genTensorInstance (N.fromList [3, 3, 4]) ''Float)
$(genTensorInstance (N.fromList [4, 3, 4]) ''Float)
$(genTensorInstance (N.fromList [2, 6, 4]) ''Float)
$(genTensorInstance (N.fromList [2, 3, 8]) ''Float)
