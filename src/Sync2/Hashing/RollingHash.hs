{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}

module Sync2.Hashing.RollingHash
  ( R, Rolling (..)
  , hash, roll
  , rsum
  , buildWord32
  , buildWord32'
  ) where

import Data.Word
import Foreign          hiding (unsafePerformIO)
import System.IO.Unsafe        (unsafePerformIO)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

-- stolen from Data.Random.Internal.Words

{-# INLINE buildWord32 #-}
buildWord32 :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
buildWord32 b0 b1 b2 b3 = unsafePerformIO . allocaBytes 4 $ \p -> do
  pokeByteOff p 0 b0
  pokeByteOff p 1 b1
  pokeByteOff p 2 b2
  pokeByteOff p 3 b3
  peek (castPtr p)

{-# INLINE buildWord32' #-}
buildWord32' :: Word16 -> Word16 -> Word32
buildWord32' w0 w1 = unsafePerformIO . allocaBytes 4 $ \p -> do
  pokeByteOff p 0 w0
  pokeByteOff p 2 w1
  peek (castPtr p)

data R = R
  { dat :: [Word8]
  , len :: !Int
  , a   :: !Word16
  , b   :: !Word16
  }

class Rolling a where
  mkR :: Int -> Int -> a -> R

instance Rolling [Word8] where
  mkR k l d = R
    { dat = d'
    , len = l
    , a   = r1 l d'
    , b   = r2 l d'
    }
   where
    d' = drop k d

instance Rolling BL.ByteString where
  mkR k l d = mkR k l (BL.unpack d)

instance Rolling BS.ByteString where
  mkR k l d = mkR k l (BS.unpack d)

hash :: R -> Word32
hash r = buildWord32' (a r) (b r)

r1 :: Int -> [Word8] -> Word16
r1 l d = go 0 0
 where
  go i !s | i < l     = go (i+1) (s + fromIntegral (d !! i))
          | otherwise = s -- mod is done automatically by choosing Word16

r2 :: Int -> [Word8] -> Word16
r2 l d = go 0 0
 where
  go i !s | i < l     = go (i+1) (s + fromIntegral (l - i) * fromIntegral (d !! i))
          | otherwise = s

-- | \"Roll\" to the next byte, i.e. increase the offset @k@ to @k+1@ and
-- calculate the next hash
roll :: R -> R
roll r = r
  { dat = drop 1 (dat r)
  , a   = a'
  , b   = b r - l * d_0 + a'
  }
 where
  -- a(k+1)
  a'  = a r - d_0 + d_l
  l   = fromIntegral $ len r
  -- d_k and d_(k+L)
  d_0 = fromIntegral $ dat r !! 0
  d_l = fromIntegral $ dat r !! len r

-- | Sum both 16 bit values for faster indexing
rsum :: R -> Word16
rsum r = a r + b r
