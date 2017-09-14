{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module MatMul.Tensor where

import Control.DeepSeq
import Data.Matrix.Static
import Data.Tensor.Static
import qualified Data.Matrix.Static as M

$(M.genMatrixInstance 4 4 ''Float)

instance NFData (Matrix 4 4 Float) where
    rnf (Tensor'4'4'Float {}) = ()
    {-# INLINE rnf #-}

mkMat :: [Float] -> Matrix 4 4 Float
mkMat = unsafeFromList

mult :: Matrix 4 4 Float -> Matrix 4 4 Float -> Matrix 4 4 Float
mult = M.mult
