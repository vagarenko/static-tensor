{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE ViewPatterns         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Tensor.Static.TH
-- Copyright   :  (C) 2017 Alexey Vagarenko
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Alexey Vagarenko (vagarenko@gmail.com)
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------

module Data.Tensor.Static.TH (
      genTensorInstance
) where

import Data.List                  (foldl')
import Data.Tensor.Static         (tensor, Tensor, unsafeFromList, toList, IsTensor)
import Language.Haskell.TH
import qualified Data.List.NonEmpty as N


-- | Generate instance for tensor and lenses for its elements.
genTensorInstance :: N.NonEmpty Int     -- ^ Dimensions of the tensor.
                  -> Name               -- ^ Type of elements.
                  -> Q [Dec]
genTensorInstance (N.toList -> dimensions) elemTypeName = do
    conName <- newName ("Tensor'" ++ concatMap (\x -> show x ++ "'") dimensions ++ nameBase elemTypeName)

    let fieldCount = product dimensions
    fieldNames <- mapM (newName . ('x' :) . show) [0 .. fieldCount - 1]

    let fields   = replicate (fromIntegral fieldCount) (Bang SourceUnpack SourceStrict, elemType)
        dims     = natListT dimensions
        elemType = ConT elemTypeName

    let dataInstDec = DataInstD
            []                       -- context
            ''Tensor                 -- family name
            [dims, elemType]         -- family params
            (Just StarT)             -- kind
            [NormalC conName fields] -- data constructor with `fieldCount` unpacked fields of type `elemType`
            []
        
    let fromListPat     = foldr (\name pat -> InfixP (VarP name) '(:) pat) WildP fieldNames
        constructTensor = foldl' (\acc name -> acc `AppE` VarE name) (ConE conName) fieldNames
        tensorPat       = ConP conName (map VarP fieldNames)
        toListBody      = ListE (map VarE fieldNames)
        failBody        = VarE 'error `AppE` LitE (StringL ("Not enough elements to build a Tensor of shape "
                                                            ++ show dimensions))

    let tensorDec          = ValD (VarP 'tensor) (NormalB $ ConE conName ) []
        unsafeFromListDec  = FunD 'unsafeFromList [ Clause [fromListPat] (NormalB constructTensor ) []
                                                  , Clause [WildP      ] (NormalB failBody        ) []]  
        toListDec          = FunD 'toList         [ Clause [tensorPat  ] (NormalB toListBody      ) []]
        tensorCInstPragmas =
            [ PragmaD (InlineP 'tensor         Inline FunLike AllPhases)
            , PragmaD (InlineP 'unsafeFromList Inline FunLike AllPhases)
            , PragmaD (InlineP 'toList         Inline FunLike AllPhases) ]

    let tensorCInstDec = InstanceD
            Nothing
            []
            (ConT ''IsTensor `AppT` dims `AppT` elemType)
            ([dataInstDec, tensorDec, unsafeFromListDec, toListDec] ++ tensorCInstPragmas)        

    pure [tensorCInstDec]

-- | Create type-level list of Nat.
natListT :: [Int] -> Type
natListT = foldr (\d acc -> PromotedConsT `AppT` LitT (NumTyLit $ fromIntegral d) `AppT` acc) PromotedNilT