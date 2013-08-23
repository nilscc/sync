{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Sync.Internal.Protocol
  ( -- * Binary network protocol
    runServer, runClient
  , ServerSettings, serverSettings, HostPreference (..)
  , ClientSettings, clientSettings
  , send, sendList, NS.streamSink, NS.withElementSink
  , receive, receiveList
  , ($=), (=$)
  , getStreamData
    -- ** Protocol buffer messages
  , sendMsg, getMsg
  , toMsg, toMsg', fromMsg
  , module Sync.Protocol.ProtoBuff
  , module Sync.Protocol.ProtoBuff.FileTransferInfo
  , module Sync.Protocol.ProtoBuff.FileLoc
  , module Sync.Protocol.ProtoBuff.MD5Hash
    -- ** Combinators
  , andReturn
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Data.ByteString (ByteString)
import Data.Conduit
import Data.Conduit.Network

import qualified Data.ByteString.Lazy         as BL
import qualified Data.Conduit.List            as CL
import qualified Data.Conduit.Network.Stream  as NS
import qualified Text.ProtocolBuffers         as PB

import Sync.Internal.Types

-- re-exports
import Sync.Protocol.ProtoBuff
import Sync.Protocol.ProtoBuff.FileTransferInfo
import Sync.Protocol.ProtoBuff.FileLoc
import Sync.Protocol.ProtoBuff.MD5Hash

runServer
  :: (MonadResourceBase m, MonadBaseControl IO m)
  => ServerSettings (ResourceT m)
  -> NetApp m ()
  -> m ()
runServer s run = runResourceT $ runTCPServer s $ \ad -> do
  sd <- NS.toStreamData ad
  runReaderT (evalStateT (run >> close) (Left sd)) sd

runClient
  :: (MonadResourceBase m, MonadBaseControl IO m)
  => ClientSettings (ResourceT m)
  -> NetApp m ()
  -> m ()
runClient s run = runResourceT $ runTCPClient s $ \ad -> do
  sd <- NS.toStreamData ad
  runReaderT (evalStateT (run >> close) (Left sd)) sd

getStreamData :: Monad m => NetApp m (NetData m)
getStreamData = lift ask

-- | Functions from "Data.Conduit.Network.Stream" lifted to the 'NetApp' monad
send :: (Monad m, NetSendable a m) => Source (NetStream m) a -> NetApp m ()
send src = do
  sd <- getStreamData
  lift.lift $ NS.send sd src

sendList :: (Monad m, NetSendable a m) => [a] -> NetApp m ()
sendList = send . CL.sourceList

-- | 'Data.Conduit.Network.Stream.receive' lifted to the 'NetApp' monad
receive
  :: (MonadResourceBase m, NetReceivable a m)
  => Sink a (NetStream m) b
  -> NetApp m b
receive sink = do
  sd <- getStreamData
  lift . lift $ NS.receive sd sink

receiveList
  :: (MonadResourceBase m, NetReceivable a m)
  => Conduit a (NetStream m) b
  -> NetApp m [b]
receiveList sink = receive $ sink =$= CL.consume

-- Close a resumable source
close :: MonadResourceBase m => NetApp m ()
close = do
  sd <- getStreamData
  lift . lift $ NS.closeStream sd

--------------------------------------------------------------------------------
-- Protocol buffer messages

toMsg
  :: (PB.ReflectDescriptor msg, PB.Wire msg, Monad m)
  => Conduit BL.ByteString m (Maybe msg)
toMsg = CL.map $ \bs ->
  case PB.messageGet bs of
       Right (m,r) | BL.null r -> Just m
       _                       -> Nothing

-- | Same as @condToMsg@ but ignore all @Nothing@s
toMsg'
  :: (PB.ReflectDescriptor msg, PB.Wire msg, Monad m)
  => Conduit BL.ByteString m msg
toMsg' = CL.mapMaybe $ \bs ->
  case PB.messageGet bs of
       Right (m,r) | BL.null r -> Just m
       _                       -> Nothing

fromMsg
  :: (PB.ReflectDescriptor msg, PB.Wire msg, Monad m)
  => Conduit msg m ByteString
fromMsg = CL.map (BL.toStrict . PB.messagePut)

getMsg
  :: (PB.ReflectDescriptor msg, PB.Wire msg, MonadResourceBase m)
  => NetApp m (Maybe msg)
getMsg = do
  r <- receive $ CL.isolate 1 =$ toMsg =$ CL.consume
  return $ case r of
    [Just x] -> Just x
    _        -> Nothing

sendMsg
  :: (PB.ReflectDescriptor msg, PB.Wire msg, MonadResourceBase m)
  => msg -> NetApp m ()
sendMsg msg = do
  send $ yield msg $= fromMsg

--------------------------------------------------------------------------------
-- Funky combinators

andReturn :: Monad m => Sink a m () -> Sink a m [a]
andReturn sink = do
  sink
  CL.consume

--------------------------------------------------------------------------------
-- Lifted functions

--(~~) :: Monad m => Source (NetStream m) a -> Sink a (NetStream m) b -> NetApp m b
--src ~~ sink = lift . lift $ src NS.~~ sink
