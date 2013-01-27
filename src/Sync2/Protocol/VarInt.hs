module Sync2.Protocol.VarInt where

import Data.Bits
import Data.Word

varint :: (Show int, Integral int) => int -> [Word8]
varint int = go (fromIntegral int :: Integer) []
 where
  go i l =
    let w8 = fromIntegral $ 127 .&. i
        r  = shiftR i 7
     in if r == 0
           then l ++ [w8 `setBit` 7]
           else go r (l ++ [w8])

fromVarint :: (Show int, Integral int, Bits int) => [Word8] -> int
fromVarint []    = 0
fromVarint [x]   = fromIntegral $ x `clearBit` 7
fromVarint (w:r) = fromIntegral w + shiftL (fromVarint r) 7
