{-# LANGUAGE RankNTypes #-}

module Sync2.Protocol
  ( send, receive
  , SizedBS, getBS, putBS
  ) where

import Data.ByteString (ByteString)
import Data.Bits
import Data.Conduit
import Data.Conduit.Network
import Data.String

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy  as BL
import qualified Data.Conduit.List     as CL
import qualified Data.Conduit.Binary   as CB

import Sync2.Protocol.VarInt

send :: Monad m => AppData m -> Sink SizedBS m ()
send ad = withRaw $ appSink ad

receive :: Monad m => AppData m -> Source m SizedBS
receive ad = appSource ad $= CL.map SizedBS

--
-- Encode from and to `ByteString'
--

-- | Fixed length ByteString packet
newtype SizedBS = SizedBS { sized_raw :: ByteString }
  deriving (Eq, Show)

instance IsString SizedBS where
  fromString s = let bs = B8.pack s
                  in SizedBS $ BS.pack (varint (BS.length bs)) `BS.append` bs

withRaw :: Monad m => Pipe ByteString ByteString o u m r -> Pipe SizedBS SizedBS o u m r
withRaw = mapInput (sized_raw) (Just . SizedBS) 

getBS :: Monad m => GLSink SizedBS m BL.ByteString
getBS = withRaw $ do
  w8s <- go []
  CB.take (fromVarint w8s)
 where
  go l = do
    c <- CB.head
    case c of
         Nothing -> return []
         Just w8 | w8 `testBit` 7 -> return (l ++ [w8])
                 | otherwise      -> go     (l ++ [w8])

putBS :: Monad m => ByteString -> Pipe l i SizedBS u m ()
putBS bs = do
  yield $ SizedBS $ BS.pack (varint (BS.length bs))
  splt bs
 where
  splt bs'
    | BS.null bs' = return ()
    | otherwise   = do
      let (f,r) = BS.splitAt 4096 bs'
      yield $ SizedBS f
      splt  r
