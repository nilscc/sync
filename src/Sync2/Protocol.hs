{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Sync2.Protocol
  ( -- * Binary network protocol
    runServer, runClient
  , ServerSettings, serverSettings, HostPreference (..)
  , ClientSettings, clientSettings
  , send1, NS.sink1, sendList, NS.sinkList, sinkList'
  , receive, NS.next
  , ($=), (=$), (~~)
  , getAppData
    -- ** Protocol buffer messages
  , sendMsg, getMsg
  , toMsg, toMsg', fromMsg
  , module Sync2.Protocol.ProtoBuff
  , module Sync2.Protocol.ProtoBuff.FileTransferInfo
  , module Sync2.Protocol.ProtoBuff.FileLoc
  , module Sync2.Protocol.ProtoBuff.MD4Hash
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

import Sync2.Protocol.Internal

-- re-exports
import Sync2.Protocol.ProtoBuff
import Sync2.Protocol.ProtoBuff.FileTransferInfo
import Sync2.Protocol.ProtoBuff.FileLoc
import Sync2.Protocol.ProtoBuff.MD4Hash

runServer
  :: (MonadResourceBase m, MonadBaseControl IO m)
  => ServerSettings (ResourceT m)
  -> NetApp m ()
  -> m ()
runServer s run = runResourceT $ runTCPServer s $ \ad ->
  runReaderT (evalStateT (run >> close) (Left ad)) ad

runClient
  :: (MonadResourceBase m, MonadBaseControl IO m)
  => ClientSettings (ResourceT m)
  -> NetApp m ()
  -> m ()
runClient s run = runResourceT $runTCPClient s $ \ad ->
  runReaderT (evalStateT (run >> close) (Left ad)) ad

getAppData :: Monad m => NetApp m (NetData m)
getAppData = lift ask

-- | Functions from "Data.Conduit.Network.Stream" lifted to the 'NetApp' monad
send1, sendList :: (Monad m, NetSendable a m) => Source (NetStream m) a -> NetApp m ()
send1 src = do
  ad <- getAppData
  lift.lift $ NS.send1 ad src

sendList src = do
  ad <- getAppData
  lift.lift $ NS.sendList ad src

sinkList'
  :: (Monad m, NetSendable a m)
  => NetData m
  -> (Sink a (NetStream m) () -> Sink b (NetStream m) c)
  -> Sink b (NetStream m) c
sinkList' nd f = do
  NS.sinkList' nd f
  

-- | 'Data.Conduit.Network.Stream.receive' lifted to the 'NetApp' monad
receive
  :: MonadResourceBase m
  => Sink BL.ByteString (NetStream m) b
  -> NetApp m b
receive sink = do
  src <- get
  (next,b) <- lift.lift $ either NS.receive NS.receive src $ sink
  put (Right next)
  return b
 --where
  --recD :: NetData m

-- Close a resumable source
close :: MonadResourceBase m => NetApp m ()
close = do
  src <- get
  either ignore close' src
 where
  close' src = lift.lift $ NS.close src
  ignore _   = return ()

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
  send1 $ yield msg $= fromMsg

--------------------------------------------------------------------------------
-- Funky combinators

andReturn :: Monad m => Sink a m () -> Sink a m [a]
andReturn sink = do
  sink
  CL.consume

--------------------------------------------------------------------------------
-- Lifted functions

(~~) :: Monad m => Source (NetStream m) a -> Sink a (NetStream m) b -> NetApp m b
src ~~ sink = lift . lift $ src NS.~~ sink
