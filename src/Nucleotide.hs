{-# language ScopedTypeVariables #-}
{-# language OverloadedStrings   #-}
{-# language BangPatterns        #-}
module Nucleotide where

import qualified Data.ByteString.Char8 as ByteString
import Data.ByteString.Char8 (ByteString)
import qualified Data.HashTable.IO as HashTable
import qualified Data.Map as M
import Text.Printf
import Data.List
import Data.Maybe
import Data.Char
import Data.IORef
import Control.Concurrent
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text


runStdIn = do
    let skip = do
            l <- ByteString.getLine
            if ByteString.isPrefixOf (ByteString.pack ">THREE") l
                then return ()
                else skip
    skip
    s <- ByteString.getContents
    let content = ByteString.map toUpper $ ByteString.filter ((/=) '\n') s;
    mapM_ (execute content) actions


run :: ByteString -> IO ()
run bs =
  let
    (_, rest) = ByteString.breakSubstring ">THREE" bs
    seq =
        ByteString.map toUpper
      . ByteString.filter (/= '\n')
      . ByteString.drop 1
      . ByteString.dropWhile (/= '\n')
      $ rest
  in
    mapM_ (execute seq) actions


data Actions = I Int | S ByteString

actions
  = [I 1, I 2, S "GGT",S "GGTA",S "GGTATT",S "GGTATTTTAATT",S "GGTATTTTAATTTATAGT"]

execute :: ByteString -> Actions -> IO ()
execute content (I i) = writeFrequencies content i
execute content (S s) = writeCount content s

writeFrequencies :: ByteString.ByteString -> Int -> IO ()
writeFrequencies input size = do
    hm <- tcalculate input size
    let
      sorted :: [(ByteString, Int)]
      sorted = sortBy (\(_,x) (_,y) -> y `compare` x) $ HashMap.toList hm
      sum :: Double
      sum = fromIntegral ((ByteString.length input) + 1 - size)
    mapM_ (\(k,v)-> do
        printf "%s %.3f\n"
            (ByteString.unpack k) ((100 * (fromIntegral v)/sum)::Double)) sorted
    putChar '\n'

writeCount :: ByteString.ByteString -> ByteString -> IO ()
writeCount input string = do
    let size = ByteString.length string
    hm <- tcalculate input size
    let
      v :: Int
      v = maybe 0 id $ HashMap.lookup string hm
    printf "%d\t%s\n" v (Text.decodeUtf8 string)

tcalculate :: ByteString.ByteString -> Int -> IO (HashMap ByteString.ByteString Int)
tcalculate input size = do
    let
      actions = map (\i -> calculate input i size 64) [0..63]
    vars <- mapM (\action -> do
                    var <- newEmptyMVar
                    forkIO $ do
                        answer <- action
                        putMVar var answer
                    return var) actions
    let result = HashMap.empty

    results :: [HashMap ByteString Int] <- mapM takeMVar vars
    return
      $ foldl' (\acc hm -> HashMap.unionWith (+) acc hm) HashMap.empty results


calculate :: ByteString.ByteString -> Int -> Int -> Int -> IO (HashMap ByteString.ByteString Int)
calculate input beg size incr = do
    let
      updateMap
        :: HashMap ByteString.ByteString (IORef Int)
        -> ByteString.ByteString
        -> IO (HashMap ByteString.ByteString (IORef Int))
      updateMap freqmap word =
           case HashMap.lookup word freqmap of
              Nothing -> do
                !ref <- newIORef 1
                let freqmap' = HashMap.insert word ref freqmap
                pure freqmap'
              Just x -> modifyIORef' x (+1) >> pure freqmap
      word inp pos = ByteString.take size . ByteString.drop pos $ inp
      calculate' :: HashMap ByteString (IORef Int) -> Int -> IO (HashMap ByteString (IORef Int))
      calculate' freqmap i
            | i >= ((ByteString.length input)+1 - size) = return freqmap
            | otherwise = do
                ht <- updateMap freqmap $ word input i
                calculate' ht (i+incr)
    let freqmap = HashMap.empty
    freqmap' <- calculate' freqmap beg
    traverse readIORef freqmap'


type HashTable k v = HashTable.BasicHashTable k v
