{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS -fno-warn-orphans #-}

module Sync.Hashing.RollingBlocks
  ( toRollingBlocks
  , toLookupWeak, findMatching, HashingMatch(..)
    -- ** Convertion & sendable instances
  ) where

import           Crypto.RollingHash
import           Crypto.RollingHash.Lookup
import qualified Crypto.RollingHash.Internal  as RH
import           Data.Conduit
import           Data.Conduit.Network.Stream
import qualified Data.Conduit.List            as CL
--import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as BL
import qualified Data.ByteString.Lazy.Builder as BUILDER
import           Data.Word

import           Sync.Internal.IO
import           Sync.Internal.Types

toRollingBlocks :: BlockSize -> BL.ByteString -> [RollingHash]
toRollingBlocks s bs = zip [0,fromIntegral s..] $ go (mkR s bs)
 where
  go !r | isEmptyR r = []
        | otherwise  = hashR32 r : go (rollBlock r)

toLookupWeak :: BlockSize -> [RollingHash] -> LookupWeak
toLookupWeak s rhsh = LookupWeak
  { lookup_map       = fromListW32 $ map switch rhsh
  , lookup_blocksize = s }
 where
  switch (x,y) = (y,x)

findMatching :: LookupWeak -> [RollingHash] -> [HashingMatch RollingHash]
findMatching lkup local_hshs = go [] local_hshs
 where
  go res [] = res
  go res ((loc, rhsh):r)
    | Just rem_loc <- lookupW32 rhsh (lookup_map lkup)
    = go (res ++ [match rem_loc]) r
    | otherwise                        = go res r
   where
    match rem_loc =
      let loc_fl = fromIntegral loc; rem_fl = fromIntegral rem_loc
       in HashingMatchWeak loc_fl rem_fl

--------------------------------------------------------------------------------
-- convertion & sendable instances

w32toBS :: Word32 -> BL.ByteString
w32toBS w32 =
  BUILDER.toLazyByteString (BUILDER.word32LE w32)

bsToW32s :: BL.ByteString -> [Word32]
bsToW32s !bs = go (BL.unpack bs)
 where
  go (a:b:c:d:r) = RH.buildWord32 a b c d : go r
  go []          = []
  go l           = error $ "Invalid Word32: " ++ show l

instance Monad m => Sendable m RollingHash where
  encode = CL.map (w32toBS . snd) =$= encode

instance Monad m => Receivable (BlockSize -> RollingHash) m where
  decode = decode =$= CL.concatMap bsToW32s =$= go 0
   where
    go n = do mw32 <- await
              case mw32 of
                   Nothing  -> return ()
                   Just w32 -> do yield (\bs -> (n * fromIntegral bs, w32))
                                  go (n+1)
