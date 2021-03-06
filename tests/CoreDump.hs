{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Algorithm.Diff
import Data.Text                    (Text)
import System.Exit
import System.Process.Typed
import Test.Tasty
import Test.Tasty.Golden.Advanced
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text    as T
import qualified Data.Text.IO as T

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup "Core Dump"
        [ testGroup "Tensor" $
            testFilesInDir "tests/CoreDump/Tensor/"
                [ "Add"
                , "Append_0"
                , "Append_1"
                , "Append_2"
                , "Cons_0"
                , "Cons_1"
                , "Cons_2"
                , "Diff"
                , "EnumFromN"
                , "EnumFromStepN"
                , "Fill"
                , "Generate"
                , "GenerateKnownNats"
                , "GenerateSing"
                , "GetSlice"
                , "GetSliceElems"
                , "GetSubtensor"
                , "GetSubtensorElems"
                , "MapSliceElems"
                , "MapSubtensorElems"
                , "Ofoldl1ExStrict"
                , "OfoldlStrict"
                , "OfoldMap"
                , "Ofoldr"
                , "Ofoldr1Ex"
                , "Omap"
                , "Ounzip"
                , "Ozip"
                , "OzipWith"
                , "Remove_0_0"
                , "Remove_1_0"
                , "Remove_2_0"
                , "Scale"
                , "SetSlice"
                , "SetSliceElems"
                , "SetSubtensor"
                , "SetSubtensorElems"
                , "Snoc_0"
                , "Snoc_1"
                , "Snoc_2"
                , "SubtensorOver"
                , "SubtensorSet"
                , "SubtensorView"
                , "TensorElemOver"
                , "TensorElemSet"
                , "TensorElemView"
                , "Zero"
                ]
        , testGroup "Vector" $
            testFilesInDir "tests/CoreDump/Vector/"
                [ "Cross"
                , "Dot"
                , "Normalize"
                , "VectorLen"
                , "VectorLenSquare"
                ]
        , testGroup "Matrix" $
            testFilesInDir "tests/CoreDump/Matrix/"
                [ "Identity"
                , "RowView"
                , "RowSet"
                , "RowOver"
                , "GetRowElems"
                , "SetRowElems"
                , "MapRowElems"
                , "ColView"
                , "ColSet"
                , "ColOver"
                , "GetColElems"
                , "SetColElems"
                , "MapColElems"
                , "Transpose"
                , "MultMatMat"
                , "MultMatVec"
                , "MultVecMat"
                , "MinorMatrix"
                , "Determinant"
                , "Minor"
                , "Cofactor"
                , "CofactorMatrix"
                , "AdjugateMatrix"
                , "Inverse"
                , "MultMatMat5"
                ]
        ]
    where
        testFilesInDir dir = map (\f -> testCoreDump $ dir ++ f)

testCoreDump :: String -> TestTree
testCoreDump name =
    goldenTest
        name
        (T.readFile $ name ++ ".dump-simpl.ghc822.golden")
        (mkCoreDump $ name)
        cmp
        (const $ pure ())
    where
        cmp golden new = pure $
            if ng == nn
                then Nothing
                else Just
                    . concatMap showDiff
                    . filter filterDiff
                    $ getDiff @(Text, Int) (ng `zip` [4..]) (nn `zip` [4..])
                    -- line numbers start from 4 because we deleted first 3 lines
            where
                ng = normalizeDump golden
                nn = normalizeDump new
                filterDiff d = case d of
                    First  _ -> True
                    Second _ -> True
                    Both _ _ -> False
                showDiff d = case d of
                    First  (t, ln) -> "Golden file @" ++ show ln ++ ":" ++ T.unpack t ++ "\n"
                    Second (t, ln) -> "New file    @" ++ show ln ++ ":" ++ T.unpack t ++ "\n"
                    Both _ _ -> ""

mkCoreDump :: String -> IO Text
mkCoreDump name = do
    let p = proc
                "cabal"
                [ "exec"
                , "ghc"
                , "--"
                , "-O2"
                , "-itests"
                , "-ddump-to-file"
                , "-ddump-simpl"
                , "-dsuppress-idinfo"
                , "-dsuppress-coercions"
                , "-dsuppress-uniques"
                , "-fforce-recomp"
                , name ++ ".hs"
                ]
    (ecode, _out, err) <- readProcess p
    case ecode of
        ExitFailure _ -> print $ TL.decodeUtf8 err
        _             -> pure ()
    T.readFile $ name ++ ".dump-simpl"

normalizeDump :: Text -> [Text]
normalizeDump t = 
    case T.lines $ T.replace "\r\n" "\n" t of
        -- Assuming "Tidy Core" header with timestamp takes the first 3 lines.
        (_ : _ : _ : xs) -> xs
        _                -> error "Incorrect dump format."