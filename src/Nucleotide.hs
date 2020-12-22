{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE BinaryLiterals             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
module Nucleotide (runStdIn) where

import qualified Control.Concurrent.ParallelIO.Global as ParallelIO
import           Data.Bits
import           Data.ByteString                      (ByteString)
import qualified Data.ByteString                      as ByteString
import qualified Data.ByteString.Char8                as Char8
import           Data.ByteString.Internal             (toForeignPtr)
import           Data.Coerce
import           Data.Hashable
import qualified Data.HashMap.Internal                as Internal
import           Data.HashMap.Strict                  (HashMap)
import qualified Data.HashMap.Strict                  as HashMap
import           Data.IORef
import           Data.List                            (foldl')
import qualified Data.Vector                          as Vector
import qualified Data.Vector.Algorithms.Intro         as Sort
import qualified Data.Vector.Storable                 as Storable
import           GHC.Generics
import           GHC.Word
import qualified Text.Builder                         as Builder


runStdIn :: IO ()
runStdIn = do
  let skip = do
          l <- ByteString.getLine
          if Char8.isPrefixOf ">THREE" l
              then return ()
              else skip
  skip
  s <- ByteString.getContents
  let sv = byteStringToVector s

  let
    {-# INLINE content #-}
    content = Storable.map (\b -> (b .&. 0b110) `shiftR` 1) . Storable.filter (/= 10) $ sv
  results <- ParallelIO.parallel (actions content)
  Builder.putToStdOut (foldl' (<>) mempty results)

actions :: Storable.Vector Word8 -> [IO Builder.Builder]
actions content =
  [ writeFrequencies1 content
  , writeFrequencies2 content
  , writeCount content "GGT"
  , writeCount content "GGTA"
  , writeCount content "GGTATT"
  , writeCount content "GGTATTTTAATT"
  , writeCount content "GGTATTTTAATTTATAGT"
  ]


writeFrequencies1 :: Storable.Vector Word8 -> IO Builder.Builder
writeFrequencies1 input = do
    hm <- calculate1B input
    mv <- Vector.unsafeThaw . Vector.fromList . HashMap.toList $ hm
    Sort.sortBy (\(_,x) (_,y) -> y `compare` x) mv
    sortedB :: Vector.Vector (Byte, Int)  <- Vector.unsafeFreeze $ mv
    let
      sorted :: Vector.Vector (Word8, Int)
      sorted = coerce sortedB
      inputLength :: Double
      inputLength = fromIntegral (Storable.length input)
    let b = Vector.foldl' (\acc (k,v)->
                      let
                        perc :: Double
                        perc = 100 * (fromIntegral v)/inputLength
                      in
                          acc
                       <> Builder.char (decode $ k)
                       <> Builder.char ' '
                       <> Builder.fixedDouble 3 perc
                       <> Builder.char '\n') mempty sorted
    pure (b <> Builder.char '\n')

writeFrequencies2 :: Storable.Vector Word8 -> IO Builder.Builder
writeFrequencies2 input = do
    hm <- calculate2B (Storable.unsafeCast input)
    mv <- Vector.unsafeThaw . Vector.fromList . HashMap.toList $ hm
    Sort.sortBy (\(_,x) (_,y) -> y `compare` x) mv
    sortedB :: Vector.Vector (Byte2, Int)  <- Vector.unsafeFreeze $ mv
    let
      sorted :: Vector.Vector (Word16, Int)
      sorted = coerce sortedB
    let
      inputLength :: Double
      inputLength  = fromIntegral (Storable.length input) - 1
    let b = foldl' (\acc (k,v)->
                      let
                        perc :: Double
                        perc = 100 * (fromIntegral v)/inputLength
                      in
                          acc
                       <> decode16 k
                       <> Builder.char ' '
                       <> Builder.fixedDouble 3 perc
                       <> Builder.char '\n') mempty  sorted
    pure (b <> Builder.char '\n')

calculate1B :: Storable.Vector Word8 -> IO (HashMap Byte Int)
calculate1B input = Storable.foldM' updateMap HashMap.empty input >>= traverse readIORef
  where
    updateMap
      :: HashMap Byte (IORef Int)
      -> Word8
      -> IO (HashMap Byte (IORef Int))
    updateMap freqmap word =
      let byte = coerce word in
         case HashMap.lookup byte freqmap of
            Nothing ->
              do
                ref <- newIORef 1
                let
                  freqmap'
                      = Internal.insertNewKey
                          (fromIntegral (hash byte))
                          byte ref freqmap
                pure freqmap'
            Just x -> modifyIORef' x (+1) >> pure freqmap


newtype Byte = Byte {getByte :: Word8}
  deriving stock Generic
  deriving newtype (Eq, Show)


instance Hashable Byte where
  hash (Byte b) = (fromIntegral b)-- .&. 0b110

decode :: Word8 -> Char
decode 0 = 'A'
decode 1 = 'C'
decode 2 = 'T'
decode 3 = 'G'
decode _ = error "decode: encountered unexpected byte"

decode16 :: Word16 -> Builder.Builder
decode16 0b0000000000000000 = Builder.text "AA"
decode16 0b0000000000000001 = Builder.text "AC"
decode16 0b0000000000000010 = Builder.text "AT"
decode16 0b0000000000000011 = Builder.text "AG"
decode16 0b0000000100000000 = Builder.text "CA"
decode16 0b0000000100000001 = Builder.text "CC"
decode16 0b0000000100000010 = Builder.text "CT"
decode16 0b0000000100000011 = Builder.text "CG"
decode16 0b0000001000000000 = Builder.text "TA"
decode16 0b0000001000000001 = Builder.text "TC"
decode16 0b0000001000000010 = Builder.text "TT"
decode16 0b0000001000000011 = Builder.text "TG"
decode16 0b0000001100000000 = Builder.text "GA"
decode16 0b0000001100000001 = Builder.text "GC"
decode16 0b0000001100000010 = Builder.text "GT"
decode16 0b0000001100000011 = Builder.text "GG"
decode16 n                  = error $ "unexpected bits: " <> show n


calculate2B :: Storable.Vector Word16 -> IO (HashMap Byte2 Int)
calculate2B input = Storable.foldM' updateMap HashMap.empty input >>= traverse readIORef
  where
    updateMap
      :: HashMap Byte2 (IORef Int)
      -> Word16
      -> IO (HashMap Byte2 (IORef Int))
    updateMap freqmap word =
      let byte2 = coerce word in
         case HashMap.lookup byte2 freqmap of
            Nothing ->
              do
                ref <- newIORef 1
                let
                  freqmap' =
                    Internal.insertNewKey (fromIntegral (hash byte2)) byte2 ref freqmap
                pure freqmap'
            Just x -> modifyIORef' x (+1) >> pure freqmap

newtype Byte2 = Byte2 {getByte2 :: Word16}
  deriving stock Generic
  deriving newtype (Eq, Show)


instance Hashable Byte2 where
  hash (Byte2 b) = (fromIntegral b)-- .&. 0b11000000110


{-# INLINE writeCount #-}
writeCount :: Storable.Vector Word8 -> ByteString -> IO Builder.Builder
writeCount input string = do
    let size = Char8.length string
    let stringV = byteStringToVector string
    let encoded = Storable.map (\b -> (b .&. 0b110) `shiftR` 1) $ stringV
    hm <- tcalculateF input size
    let
      v :: Int
      v = maybe 0 id $ HashMap.lookup (coerce encoded) hm
    let b = Builder.unsignedDecimal v <> Builder.char '\t' <> Builder.asciiByteString string
    pure (b <> Builder.char '\n')

tcalculateF :: Storable.Vector Word8 -> Int -> IO (HashMap Incremental Int)
tcalculateF input size = do
    let
      computeHashMaps = map (\i -> calculateF input i size 96) [0..95]
    results <- ParallelIO.parallel computeHashMaps
    return
      $ foldl' (\ !acc !hm -> HashMap.unionWith (+) acc hm) HashMap.empty results



calculateF :: Storable.Vector Word8 -> Int -> Int -> Int -> IO (HashMap Incremental Int)
calculateF input !beg !size !incr = do
    let
      updateMap
        :: HashMap Incremental (IORef Int)
        -> Incremental
        -> IO (HashMap Incremental (IORef Int))
      updateMap freqmap word =
           case HashMap.lookup word freqmap of
              Nothing -> do
                ref <- newIORef 1
                let freqmap' = Internal.insertNewKey (fromIntegral (hash word)) word ref freqmap
                pure freqmap'
              Just x -> modifyIORef' x (+1) >> pure freqmap
      {-# INLINE calculate' #-}
      calculate' :: HashMap Incremental (IORef Int) -> Int -> IO (HashMap Incremental (IORef Int))
      calculate' freqmap i
            | i >= ((Storable.length input)+1 - size) = return freqmap
            | otherwise = do
                ht <- updateMap freqmap $ coerce (Storable.unsafeSlice i size input)
                calculate' ht (i+incr)
    freqmap' <- calculate' HashMap.empty beg
    traverse readIORef freqmap'


instance Hashable Incremental where
  hash (Incremental v) =
    Storable.foldl'
    (\ acc w -> (acc `shiftL` 2) .|. (fromIntegral w))-- .&. 0b110))
    0
    v
  hashWithSalt _ = hash

newtype Incremental = Incremental {getIncremental :: Storable.Vector Word8}
  deriving stock Generic
  deriving newtype Eq


byteStringToVector :: ByteString -> Storable.Vector Word8
byteStringToVector bs = vec where
    vec = Storable.unsafeFromForeignPtr fptr off len
    (fptr, off, len) = toForeignPtr bs
