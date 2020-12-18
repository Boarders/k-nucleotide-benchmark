{-# language BlockArguments #-}
module Main where

import qualified Criterion.Main as Criterion
import qualified NucleotideOld as Old
import qualified Nucleotide as New
import qualified Data.ByteString.Char8 as ByteString
import Data.ByteString.Char8 (ByteString)

nucleotide :: Criterion.Benchmark
nucleotide =
  Criterion.env getInput $ \ ~bs ->  
  Criterion.bgroup "nucleotides"
    [
--      Criterion.bench "nucleotide_old"  $ Criterion.nfIO (Old.run bs)
--    ,
      Criterion.bench "nucleotide_new"  $ Criterion.nfIO (New.run bs)
    ]


getInput :: IO ByteString
getInput = do
  ByteString.readFile "input/nucleo.txt"

main :: IO ()
main = do
  Criterion.defaultMain . pure $
    nucleotide
