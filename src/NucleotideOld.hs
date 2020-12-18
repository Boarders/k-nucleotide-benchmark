{-# language ScopedTypeVariables #-}
{-# language OverloadedStrings #-}
module NucleotideOld where

import qualified Data.ByteString.Char8 as ByteString
import Data.ByteString.Char8 (ByteString)
import qualified Data.HashTable.IO as H
import qualified Data.Map as M
import Text.Printf
import Data.List
import Data.Maybe
import Data.Char
import Data.IORef
import Control.Concurrent

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
  
  
data Actions = I Int | S String
actions = [I 1,I 2,
           S "GGT",S "GGTA",S "GGTATT",S "GGTATTTTAATT",S "GGTATTTTAATTTATAGT"]
execute content (I i) = writeFrequencies content i
execute content (S s) = writeCount content s

writeFrequencies :: ByteString.ByteString -> Int -> IO ()
writeFrequencies input size = do
    mp <- tcalculate input size
    let sorted = sortBy (\(_,x) (_,y) -> y `compare` x) $ M.toList mp
        sum = fromIntegral ((ByteString.length input) + 1 - size)
    mapM_ (\(k,v)-> do
        printf "%s %.3f\n"
            (ByteString.unpack k) ((100 * (fromIntegral v)/sum)::Double)) sorted
    putChar '\n'

writeCount :: ByteString.ByteString -> String -> IO ()
writeCount input string = do
    let size = length string
    mp <- tcalculate input size
    let
      v :: Int
      v = maybe 0 id $ M.lookup (ByteString.pack string) mp
    printf "%d\t%s\n" v string

tcalculate :: ByteString.ByteString -> Int -> IO (M.Map ByteString.ByteString Int)
tcalculate input size = do
    let
        l = [0..63]
        actions = map (\i -> calculate input i size (length l)) l
    vars <- mapM (\action -> do
                    var <- newEmptyMVar
                    forkIO $ do
                        answer <- action
                        putMVar var answer
                    return var) actions
    let result = M.empty
    results <- mapM takeMVar vars
    return $ foldl (\res m -> foldl
                               (\m (k,v)->M.insertWith (+) k v m)
                                res m)
                   result results

calculate :: ByteString.ByteString -> Int -> Int -> Int -> IO [(ByteString.ByteString,Int)]
calculate input beg size incr = do
    let
      updateMap
        :: HashTable ByteString.ByteString (IORef Int)
        -> ByteString.ByteString
        -> IO (HashTable ByteString.ByteString (IORef Int))
      updateMap freqmap word = do
           lu <- H.lookup freqmap word
           case lu of
            Nothing -> do
                ref <- newIORef 1
                H.insert freqmap word ref
            Just x -> modifyIORef' x (+1)
           return freqmap
      word inp pos sz = ByteString.take size $ ByteString.drop pos inp
      calculate' freqmap i
            | i >= ((ByteString.length input)+1 - size) = return ()
            | otherwise = do
                ht <- updateMap freqmap $ word input i size
                calculate' ht (i+incr)
    freqmap <- H.new :: IO (HashTable ByteString.ByteString (IORef Int))
    calculate' freqmap beg
    lst <- H.toList freqmap
    mapM (\(x,y)-> do
            v <- readIORef y
            return (x,v)) lst

type HashTable k v = H.BasicHashTable k v
