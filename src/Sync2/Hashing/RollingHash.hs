{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}

module Sync2.Hashing.RollingHash
  ( R, Rolling (..)
  , hash, roll, skipBlock, isEmptyR
  , rsum
  , buildWord32
  , buildWord32'
  , rPosition, rBlockSize, rFileLocation, rGetBlock
  ) where

import Data.Word
import Foreign          hiding (unsafePerformIO)
import System.IO.Unsafe        (unsafePerformIO)

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL

import Sync2.Protocol.ProtoBuff.FileLoc

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
  { dat      :: [Word8]
  , dat_left :: Integer
  , dat_len  :: Integer
  , len      :: !Int
  , a        :: !Word16
  , b        :: !Word16
  }
  deriving Show

class Rolling a where
  mkR
    :: Int        -- ^ Number of bytes to skip
    -> Int        -- ^ Block length
    -> Integer    -- ^ Total length
    -> a          -- ^ Data
    -> R

hasN :: Int -> [a] -> Bool
hasN 0 _     = True
hasN _ []    = False
hasN n (_:r) = hasN (n-1) r

emptyR :: R
emptyR = R [] 0 0 0 0 0

instance Rolling [Word8] where
  mkR k l l_tot d
    | hasN (k+l) d = R
      { dat      = d'
      , dat_left = left
      , dat_len  = l_tot
      , len      = l'
      , a        = r1 l' d'
      , b        = r2 l' d'
      }
    | otherwise = emptyR
   where
    d' = drop k d
    left = l_tot - fromIntegral k
    -- make sure we dont exceed any limits
    l' = minimum [l, fromIntegral left]

instance Rolling BL.ByteString where
  mkR k l l_tot d = mkR k l l_tot (BL.unpack d)

instance Rolling BS.ByteString where
  mkR k l l_tot d = mkR k l l_tot (BS.unpack d)

-- | Hash current block weakly
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
-- calculate the next hash. Returns 'emptyR' when done.
roll :: R -> R
roll r
  | isEmptyR r = emptyR
  | otherwise  = r
    { dat      = tail (dat r)
    , a        = a'
    , b        = b r - fromIntegral l' * d_0 + a'
    , dat_left = left' -- dat_left r - 1
    , len      = l'
    }
 where
  -- a(k+1)
  a'    = a r - d_0 + d_l
  l'    = minimum [len r, fromIntegral left']
  left' = dat_left r - 1
  -- d_k and d_(k+L)
  d_0 = fromIntegral $ dat r !! 0
  d_l = fromIntegral $ dat r !! l'

-- | Skip a block of the current block length. Returns 'emptyR' when done.
skipBlock :: R -> R
skipBlock r@R{ dat = d, dat_left = l_tot, len = l }
  | l_tot' <= 0 = emptyR
  | otherwise   = r -- TODO: re-use 'mkR'
    { dat = d'
    , dat_left = l_tot'
    , len = l'
    , a = r1 l' d'
    , b = r2 l' d'
    }
 where
  l_tot' = l_tot - fromIntegral l
  -- length might change if there are less than @l@ bytes left
  l'     = minimum [ l
                   -- make sure we don't exceed 'Int' limits here (even though
                   -- we probably never will...)
                   , fromIntegral (minimum [l_tot', fromIntegral (maxBound :: Int)])]
  d' = drop l d

isEmptyR :: R -> Bool
isEmptyR r = dat_left r <= 0 || null (dat r)

-- | Sum both 16 bit values for faster indexing
rsum :: R -> Word16
rsum r = a r + b r

-- | Return current position in file
rPosition :: R -> Integer
rPosition r = dat_len r - dat_left r

rBlockSize :: R -> Int
rBlockSize r = len r

-- | Return file location of current R (if not 'emptyR')
rFileLocation :: R -> FileLoc
rFileLocation r = FileLoc pos size
 where
  pos  = fromIntegral $ rPosition r
  size = fromIntegral $ rBlockSize r

rGetBlock :: R -> [Word8]
rGetBlock R{ dat = d, len = l } = take l d
