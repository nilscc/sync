module Sync2.Hashing
  ( BlockSize, condEncRolling, condDecRolling
  ) where

import Control.Monad
import Data.Conduit
import Data.Word

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy         as BL
import qualified Data.Conduit.Binary          as CB
import qualified Data.ByteString.Lazy.Builder as BUILDER

import Sync2.Protocol
import Sync2.Hashing.RollingHash

--import Sync2.ProtoBuff.FileTransferInfo

type BlockSize = Int

condEncRolling :: Monad m => BlockSize -> Conduit ByteString m SizedBS
condEncRolling s = condEncRolling' s =$= w32toSizedBS

condEncRolling' :: Monad m => BlockSize -> Conduit ByteString m Word32
condEncRolling' s =
  loop
 where
  loop = do
    b <- CB.take s
    if BL.null b then
       return ()
     else do
       yield $ hash (mkR 0 (fromIntegral $ BL.length b) b)
       loop

w32toSizedBS :: Monad m => Conduit Word32 m SizedBS
w32toSizedBS = awaitForever $ \w32 ->
  putBS $ BL.toStrict $
    BUILDER.toLazyByteString (BUILDER.word32LE w32)

condDecRolling :: Monad m => Conduit SizedBS m Word32
condDecRolling =
  loop
 where
  loop = do
    bs <- getBS
    unless (BL.null bs) $ do
      yield $ tow32 (BL.unpack bs)
      loop
  tow32 [a,b,c,d] = buildWord32 a b c d
  tow32 _         = error "asd"
