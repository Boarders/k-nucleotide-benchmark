{-# language BlockArguments #-}
module Main where

import qualified Criterion.Main as Criterion
import qualified NucleotideOld as Old

nucleotideOld :: Criterion.Benchmark
nucleotideOld =
  Criterion.bgroup "nucleotide_old"
    [ Criterion.bench "nucleotide_old"  $ Criterion.nfIO Old.runStdIn
    ]

main :: IO ()
main = do
  Criterion.defaultMain . pure $
    nucleotideOld
