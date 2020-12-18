{-# language ScopedTypeVariables #-}
{-# language OverloadedStrings   #-}
{-# language BangPatterns        #-}
{-# language TypeApplications    #-}
{-# language DeriveGeneric       #-}
{-# language FlexibleContexts    #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language DerivingStrategies  #-}
module Nucleotide (run) where

import qualified Data.ByteString.Char8 as Char8
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.ByteString.Internal (w2c, ByteString(PS), accursedUnutterablePerformIO)
import Text.Printf
import Data.List
import Data.Char
import Data.IORef
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Control.Concurrent.ParallelIO.Global as ParallelIO
import GHC.Word
import Data.Coerce
import Data.Bits (xor)
import Data.Hashable
import GHC.Generics
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Storable (peekByteOff)
import qualified Text.Builder as Builder
import Data.Foldable (fold)


runStdIn = do
  let skip = do
          l <- Char8.getLine
          if Char8.isPrefixOf (Char8.pack ">THREE") l
              then return ()
              else skip
  skip
  s <- Char8.getContents
  let content = Char8.map toUpper $ Char8.filter ((/=) '\n') s;
  res <- mapM (execute content) actions
  Builder.putToStdOut (fold res)


run :: ByteString -> IO ()
run bs = do
  let
    (_, rest) = Char8.breakSubstring ">THREE" bs
    seq =
        Char8.map toUpper
      . Char8.filter (/= '\n')
      . Char8.drop 1
      . Char8.dropWhile (/= '\n')
      $ rest
  res <- mapM (execute seq) actions
  Builder.putToStdOut (fold res)



data Actions = One | Two | S ByteString

actions
  = [One, Two, S "GGT",S "GGTA",S "GGTATT",S "GGTATTTTAATT",S "GGTATTTTAATTTATAGT"]

execute :: ByteString -> Actions -> IO Builder.Builder
execute content One   = writeFrequencies1 content
execute content Two   = writeFrequencies2 content
execute content (S s) = writeCount content s

writeFrequencies :: ByteString -> Int -> IO Builder.Builder
writeFrequencies input size = do
    hm <- tcalculateF input size
    let
      sorted :: [(ByteString, Int)]
      sorted = sortBy (\(_,x) (_,y) -> y `compare` x) $ coerce . HashMap.toList $ hm
      sum :: Double
      sum = fromIntegral ((Char8.length input) + 1 - size)
    let b = foldl' (\acc (k,v)->
                      let
                        perc :: Double
                        perc = 100 * (fromIntegral v)/sum
                      in
                          acc
                       <> Builder.asciiByteString k
                       <> Builder.char ' '
                       <> Builder.fixedDouble 3 perc) mempty sorted
    pure (b <> Builder.char '\n')



writeFrequencies1 :: ByteString -> IO Builder.Builder
writeFrequencies1 input = do
    hm <- tcalculate1 input
    let
      sorted :: [(Word8, Int)]
      sorted = sortBy (\(_,x) (_,y) -> y `compare` x) $ coerce . HashMap.toList $ hm
      sum :: Double
      sum = fromIntegral (Char8.length input)
    let b = foldl' (\acc (k,v)->
                      let
                        perc :: Double
                        perc = 100 * (fromIntegral v)/sum
                      in
                          acc
                       <> Builder.char (w2c k)
                       <> Builder.char ' '
                       <> Builder.fixedDouble 3 perc
                       <> Builder.char '\n') mempty sorted
    pure (b <> Builder.char '\n')

writeFrequencies2 :: ByteString -> IO Builder.Builder
writeFrequencies2 input = do
    hm <- tcalculate2 input
    let
      sorted :: [(Word16, Int)]
      sorted = sortBy (\(_,x) (_,y) -> y `compare` x) $ coerce . HashMap.toList $ hm
      sum :: Double
      sum = fromIntegral (Char8.length input)
    let b = foldl' (\acc (k,v)->
                      let
                        perc :: Double
                        perc = 100 * (fromIntegral v)/sum
                      in
                          acc
                       <> showBytes16 k
                       <> Builder.char ' '
                       <> Builder.fixedDouble 3 perc
                       <> Builder.char '\n') mempty sorted
    pure (b <> Builder.char '\n')


writeCount :: Char8.ByteString -> ByteString -> IO Builder.Builder
writeCount input string = do
    let size = Char8.length string
    hm <- tcalculate input size
    let
      v :: Int
      v = maybe 0 id $ HashMap.lookup (coerce string) hm
    let b = Builder.unsignedDecimal v <> Builder.char '\t' <> Builder.asciiByteString string
    pure (b <> Builder.char '\n')
--    printf "%d\t%s\n" v (Text.decodeUtf8 string)

tcalculate :: Char8.ByteString -> Int -> IO (HashMap Char8.ByteString Int)
tcalculate input size = do
    let actions = map (\i -> calculate input i size 64) [0..63]
    results <- ParallelIO.parallel actions
    return
      $ foldl' (\acc hm -> HashMap.unionWith (+) acc hm) HashMap.empty results

tcalculate1 :: Char8.ByteString -> IO (HashMap Word8 Int)
tcalculate1 input = do
    let actions = map (\i -> calculate1 input i 64) [0..63]
    results <- ParallelIO.parallel actions
    return
      $ foldl' (\acc hm -> HashMap.unionWith (+) acc hm) HashMap.empty results


tcalculate2 :: Char8.ByteString -> IO (HashMap Word16 Int)
tcalculate2 input = do
    let actions = map (\i -> calculate2 input i 64) [0..63]
    results <- ParallelIO.parallel actions
    return
      $ foldl' (\acc hm -> HashMap.unionWith (+) acc hm) HashMap.empty results


tcalculateF :: Char8.ByteString -> Int -> IO (HashMap FNV1 Int)
tcalculateF input size = do
    let actions = map (\i -> calculateF input i size 64) [0..63]
    results <- ParallelIO.parallel actions

    return
      $ foldl' (\acc hm -> HashMap.unionWith (+) acc hm) HashMap.empty results


calculate :: Char8.ByteString -> Int -> Int -> Int -> IO (HashMap Char8.ByteString Int)
calculate input beg size incr = do
    let
      updateMap
        :: HashMap Char8.ByteString (IORef Int)
        -> Char8.ByteString
        -> IO (HashMap Char8.ByteString (IORef Int))
      updateMap freqmap word =
           case HashMap.lookup word freqmap of
              Nothing -> do
                ref <- newIORef 1
                let freqmap' = HashMap.insert word ref freqmap
                pure freqmap'
              Just x -> modifyIORef' x (+1) >> pure freqmap
      word inp pos = Char8.take size . Char8.drop pos $ inp
      calculate' :: HashMap ByteString (IORef Int) -> Int -> IO (HashMap ByteString (IORef Int))
      calculate' freqmap i
            | i >= ((Char8.length input)+1 - size) = return freqmap
            | otherwise = do
                ht <- updateMap freqmap $ word input i
                calculate' ht (i+incr)
    freqmap' <- calculate' HashMap.empty beg
    traverse readIORef freqmap'

calculate1 :: ByteString -> Int -> Int -> IO (HashMap Word8 Int)
calculate1 input beg incr = do
    let
      updateMap
        :: HashMap Word8 (IORef Int)
        -> Word8
        -> IO (HashMap Word8 (IORef Int))
      updateMap freqmap word =
           case HashMap.lookup word freqmap of
              Nothing -> do
                ref <- newIORef 1
                let freqmap' = HashMap.insert word ref freqmap
                pure freqmap'
              Just x -> modifyIORef' x (+1) >> pure freqmap
      word inp pos = ByteString.index inp pos
      calculate' :: HashMap Word8 (IORef Int) -> Int -> IO (HashMap Word8 (IORef Int))
      calculate' freqmap i
            | i >= (ByteString.length input) = return freqmap
            | otherwise = do
                ht <- updateMap freqmap $ word input i
                calculate' ht (i+incr)
    freqmap' <- calculate' HashMap.empty beg
    traverse readIORef freqmap'

index16 :: ByteString -> Int -> Word16
index16 ps n
  | n < 0  = error $ "index" ++ ("negative index: " ++ show n)
  | n >= ByteString.length ps
     = error
       $ "index" ++ ("index too large: " ++ show n ++ ", length = " ++ show (ByteString.length ps))
    | otherwise      = ps `unsafeIndex16` n
{-# INLINE index16 #-}

unsafeIndex16 :: ByteString -> Int -> Word16
unsafeIndex16 (PS x s l) i =
    accursedUnutterablePerformIO $
      withForeignPtr x $ \p -> peekByteOff p (s+i)
{-# INLINE unsafeIndex16 #-}


calculate2 :: ByteString -> Int -> Int -> IO (HashMap Word16 Int)
calculate2 input beg incr = do
    let
      updateMap
        :: HashMap Word16 (IORef Int)
        -> Word16
        -> IO (HashMap Word16 (IORef Int))
      updateMap freqmap word =
           case HashMap.lookup word freqmap of
              Nothing -> do
                ref <- newIORef 1
                let freqmap' = HashMap.insert word ref freqmap
                pure freqmap'
              Just x -> modifyIORef' x (+1) >> pure freqmap
      word inp pos = index16 inp pos
      calculate' :: HashMap Word16 (IORef Int) -> Int -> IO (HashMap Word16 (IORef Int))
      calculate' freqmap i
            | i >= (ByteString.length input - 1) = return freqmap
            | otherwise = do
                ht <- updateMap freqmap $ word input i
                calculate' ht (i+incr)
    freqmap' <- calculate' HashMap.empty beg
    traverse readIORef freqmap'


fnv1 :: FNV1 -> Int
fnv1 = coerce $ \bs -> ByteString.foldl'
  (\acc w -> (fromIntegral @Word8 @Int w `xor` acc) * 0x00000100000001B3
  ) 0xcbf29ce484222325 bs


newtype FNV1 = FNV1 {getFNV1 :: ByteString}
  deriving stock (Generic)
  deriving newtype (Eq)

instance Hashable FNV1 where
  hash = fnv1

calculateF :: Char8.ByteString -> Int -> Int -> Int -> IO (HashMap FNV1 Int)
calculateF input beg size incr = do
    let
      updateMap
        :: HashMap FNV1 (IORef Int)
        -> FNV1
        -> IO (HashMap FNV1 (IORef Int))
      updateMap freqmap word =
           case HashMap.lookup word freqmap of
              Nothing -> do
                ref <- newIORef 1
                let freqmap' = HashMap.insert word ref freqmap
                pure freqmap'
              Just x -> modifyIORef' x (+1) >> pure freqmap
      word inp pos = coerce . Char8.take size . Char8.drop pos $ inp
      calculate' :: HashMap FNV1 (IORef Int) -> Int -> IO (HashMap FNV1 (IORef Int))
      calculate' freqmap i
            | i >= ((Char8.length input)+1 - size) = return freqmap
            | otherwise = do
                ht <- updateMap freqmap $ word input i
                calculate' ht (i+incr)
    let freqmap = HashMap.empty
    freqmap' <- calculate' freqmap beg
    traverse readIORef freqmap'

showBytes16 :: Word16 -> Builder.Builder
showBytes16 16705 = Builder.text   "AA"
showBytes16 16724 = Builder.text   "TA"
showBytes16 21569 = Builder.text   "AT"
showBytes16 21588 = Builder.text   "TT"
showBytes16 16707 = Builder.text   "CA"
showBytes16 17217 = Builder.text   "AC"
showBytes16 18241 = Builder.text   "AG"
showBytes16 16711 = Builder.text   "GA"
showBytes16 21571 = Builder.text   "CT"
showBytes16 17236 = Builder.text   "TC"
showBytes16 21575 = Builder.text   "GT"
showBytes16 18260 = Builder.text   "TG"
showBytes16 17219 = Builder.text   "CC"
showBytes16 17223 = Builder.text   "GC"
showBytes16 18243 = Builder.text   "CG"
showBytes16 18247 = Builder.text   "GG"
showBytes16 n = error $ "showBytes16: encoutered " ++ (show $ n)
