module Main where

import Criterion.Main
import Control.Monad
import System.Random.MWC
import qualified MatMul.Tensor as T
import qualified MatMul.Linear as L
import qualified MatMul.Unrolled as U
import qualified MatMul.UnrolledFull as F
import qualified MatMul.Vector as V
import qualified MatMul.Vector4 as V4

main :: IO ()
main = do
    gen <- create
    es1 <- mkElems gen
    es2 <- mkElems gen

    defaultMain
        [ bgroup "matrix mult 4x4"
            [ bench "tensor"       $ nf ( T.mult ( T.mkMat es1)) ( T.mkMat es2)
            , bench "linear"       $ nf ( L.mult ( L.mkMat es1)) ( L.mkMat es2)
            , bench "unrolled"     $ nf ( U.mult ( U.mkMat es1)) ( U.mkMat es2)
            , bench "unrolledFull" $ nf ( F.mult ( F.mkMat es1)) ( F.mkMat es2)
            , bench "vector"       $ nf ( V.mult ( V.mkMat es1)) ( V.mkMat es2)
            , bench "vector4"      $ nf (V4.mult (V4.mkMat es1)) (V4.mkMat es2)
            ]
        ]
    where
        mkElems gen = replicateM 16 (fmap (\x -> x - (2 :: Float) ^^ (-33 :: Int)) (uniform gen :: IO Float))