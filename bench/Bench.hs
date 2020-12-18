{-# language BlockArguments #-}
module Main (main) where

import Criterion.Main
import qualified NucleotideOld as Old
import qualified Nucleotide as New
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Char8 (ByteString)

main :: IO ()
main = defaultMain
  [ env setup $ \ ~(s,m,l) ->
    bgroup "nucleotides"
    [ bgroup "new"
      [ bench "small" $ nfIO (New.run s)
      , bench "medium" $ nfIO (New.run m)
      , bench "large" $ nfIO (New.run l)
      ]
    , bgroup "old"
      [ bench "small" $ nfIO (New.run s)
      , bench "medium" $ nfIO (New.run m)
      , bench "large" $ nfIO (New.run l)
      ]
    ]
  ]


setup :: IO (ByteString, ByteString, ByteString)
setup = do
  s <- BS8.readFile "input/small.fasta"
  m <- BS8.readFile "input/medium.fasta"
  l <- BS8.readFile "input/large.fasta"
  return (s,m,l)
