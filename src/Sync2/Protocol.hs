{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Sync2.Protocol
  ( -- * Binary network protocol
    runServer, runClient
  , ServerSettings, serverSettings, HostPreference (..)
  , ClientSettings, clientSettings
  , send, receive
  , ($=), (=$), ($$), ($$+), ($$++), ($$+-)
    -- ** Protocol buffer messages
  , sendMsg, getMsg
  , condToMsg, condToMsg', condFromMsg
  , module Sync2.Protocol.ProtoBuff
  , module Sync2.Protocol.ProtoBuff.FileTransferInfo
  , module Sync2.Protocol.ProtoBuff.FileLoc
  , module Sync2.Protocol.ProtoBuff.MD4Hash
    -- ** Combinators
  , andReturn
  ) where

import Control.Monad
import Control.Monad.Reader
import Data.ByteString (ByteString)
import Data.Bits
import Data.Conduit
import Data.Conduit.Network

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as BL
import qualified Data.Conduit.List     as CL
import qualified Data.Conduit.Binary   as CB
import qualified Text.ProtocolBuffers  as PB

import Sync2.Protocol.Internal
import Sync2.Protocol.VarInt

-- re-exports
import Sync2.Protocol.ProtoBuff
import Sync2.Protocol.ProtoBuff.FileTransferInfo
import Sync2.Protocol.ProtoBuff.FileLoc
import Sync2.Protocol.ProtoBuff.MD4Hash

runServer
  :: (MonadIO m, MonadBaseControl IO m)
  => ServerSettings m
  -> NetApp m ()
  -> m ()
runServer s run = runTCPServer s $ runReaderT $ runResourceT run

runClient
  :: (MonadIO m, MonadBaseControl IO m)
  => ClientSettings m
  -> NetApp m ()
  -> m ()
runClient s run = runTCPClient s $ runReaderT $ runResourceT run

send :: MonadIO m => Sink ByteString (NetApp m)  ()
send = do
  ad <- lift ask
  condShow "send: " =$ condFromBS =$ withRaw (transPipe (lift.lift) $ appSink ad)

receive :: MonadIO m => Source (NetApp m) BL.ByteString
receive = do
  ad <- lift ask
  transPipe (lift.lift) (appSource ad) $= CL.map SizedBS $= condToBS $= condShow "recv: "

--type NetM m = ReaderT (AppData (NetM m))

--runClient :: ClientSett

--------------------------------------------------------------------------------
-- Encode from and to `ByteString'

type Get m a = GLSink SizedBS m a

-- | Get ByteString from the stream
getBS :: Monad m => Get m BL.ByteString
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

type Put m = forall l i u. Pipe l i SizedBS u m ()

-- | Put a ByteString into the stream
putBS :: Monad m => ByteString -> Put m
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

condFromBS :: Monad m => Conduit ByteString m SizedBS
condFromBS = awaitForever putBS

condToBS :: Monad m => Conduit SizedBS m BL.ByteString
condToBS = CL.sequence getBS

--------------------------------------------------------------------------------
-- Lists

putListStart, putListEnd :: Monad m => Put m
putListStart = yield $ SizedBS $ BS.pack [0, 0 `setBit` 7]
putListEnd   = yield $ SizedBS $ BS.pack [1, 0 `setBit` 7]

getList :: Monad m => Get m

--------------------------------------------------------------------------------
-- Protocol buffer messages

condToMsg
  :: (PB.ReflectDescriptor msg, PB.Wire msg, Monad m)
  => Conduit BL.ByteString m (Maybe msg)
condToMsg = CL.map $ \bs ->
  case PB.messageGet bs of
       Right (m,r) | BL.null r -> Just m
       _                       -> Nothing

-- | Same as @condToMsg@ but ignore all @Nothing@s
condToMsg'
  :: (PB.ReflectDescriptor msg, PB.Wire msg, Monad m)
  => Conduit BL.ByteString m msg
condToMsg' = CL.mapMaybe $ \bs ->
  case PB.messageGet bs of
       Right (m,r) | BL.null r -> Just m
       _                       -> Nothing

condFromMsg
  :: (PB.ReflectDescriptor msg, PB.Wire msg, Monad m)
  => Conduit msg m ByteString
condFromMsg = CL.map (BL.toStrict . PB.messagePut)

-- | Send protocol buffer messages
sendMsg
  :: (PB.ReflectDescriptor msg, PB.Wire msg, MonadIO m)
  => Sink msg (NetApp m) ()
sendMsg = condFromMsg =$ send

-- | Receive protocol buffer messages
getMsg
  :: (PB.ReflectDescriptor msg, PB.Wire msg, Monad m)
  => Sink BL.ByteString m (Maybe msg)
getMsg = condToMsg =$ join `fmap` await

--------------------------------------------------------------------------------
-- Funky combinators

andReturn :: Monad m => Sink a m () -> Sink a m [a]
andReturn sink = do
  sink
  CL.consume
