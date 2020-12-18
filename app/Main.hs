module Main where

import qualified NucleotideOld as Old
import qualified Nucleotide as New
import qualified Data.ByteString.Char8 as ByteString

main :: IO ()
main = do
  bs <- ByteString.getContents
  New.run bs
