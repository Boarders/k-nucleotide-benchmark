module Main where

import NucleotideOld
import qualified Data.ByteString.Char8 as ByteString

main :: IO ()
main = do
  bs <- ByteString.getContents
  run bs
