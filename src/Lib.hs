module Lib
where

import System.IO
import Control.Exception
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Safe (lastMay, headMay)
import Data.Word (Word32)
import Data.Monoid
import qualified Data.Binary.Builder as Builder

intoWords :: BL.ByteString -> [ BL.ByteString ]
intoWords bs
  | BL.null a = []
  | otherwise = a : intoWords b
  where (a,b) = BL.splitAt 4 bs

wanted :: BL.ByteString -> Bool
wanted bs = BL.index bs 0 == 0xC0

toWord32 :: BS.ByteString -> Word32
toWord32 bs =
  let (a : b : c : d : _) = BS.unpack bs
      fi = fromIntegral
  in fi d + 256*(fi c + 256*(fi b + 256*fi a))

toWord32lazy :: BL.ByteString -> Word32
toWord32lazy bs =
  let (a : b : c : d : _) = BL.unpack bs
      fi = fromIntegral
  in fi d + 256*(fi c + 256*(fi b + 256*fi a))

-- find by breaking the file into 4-byte words
find_C0_v1 :: FilePath -> IO (Maybe BL.ByteString)
find_C0_v1 path = do
  contents <- BL.readFile path
  return $ lastMay . filter (\bs -> BL.index bs 0 == 0xC0) . intoWords $ contents

-- find by looking at every 4th byte
find_C0_v2 :: FilePath -> IO (Maybe BL.ByteString)
find_C0_v2 path = do
  contents <- BL.readFile path
  let size = BL.length contents
      wordAt i = BL.take 4 . BL.drop i $ contents
  return $ fmap wordAt $ lastMay $ filter (\i -> BL.index contents i == 0xC0) [0,4..size]

-- read a file backwords until a predicate returns a Just value
loopBlocks :: Int -> Handle -> Integer -> (BS.ByteString -> Integer -> Maybe a) -> IO (Maybe a)
loopBlocks blksize h top pred
  | top <= 0 = return Nothing
  | otherwise   = do
        let offset = top - fromIntegral blksize
        hSeek h AbsoluteSeek offset
        blk <- BS.hGet h blksize
        case pred blk offset of 
          Nothing -> loopBlocks blksize h offset pred
          x       -> return x

-- find by reading backwords lookint at every 4th byte
find_C0_v3 :: FilePath -> IO (Maybe Integer)
find_C0_v3 path = do
  withFile path ReadMode $ \h -> do
    size <- hFileSize h
    let top = size - (mod size 4)
        blksize = 64*1024 :: Int
    loopBlocks blksize h top $ \blk offset ->
          fmap ( (+offset) . fromIntegral ) $ headMay $ filter (\i -> BS.index blk i == 0xC0) [blksize-4,blksize-8..0]

create_C0_file :: FilePath -> Int -> IO ()
create_C0_file path count = do
  let toWord32 i = 0xc0*256*256*256 + fromIntegral (mod i (256*256*256))
      builder = mconcat [ Builder.putWord32be (toWord32 i) | i <- [0..count-1] ]
  withFile path WriteMode $ \h -> do
    BL.hPut h (Builder.toLazyByteString builder)

