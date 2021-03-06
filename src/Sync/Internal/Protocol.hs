{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Sync.Internal.Protocol where

import Control.Exception
import Control.Monad.Reader
import Control.Monad.State
import Data.ByteString (ByteString)
import Data.Conduit

import qualified Data.ByteString.Lazy         as BL
import qualified Data.Conduit.List            as CL
import qualified Data.Conduit.Network         as CN
import qualified Data.Conduit.Network.Stream  as NS
import qualified Text.ProtocolBuffers         as PB

import Sync.Internal.Types
import Sync.Internal.Exceptions

runServer
  :: (MonadResourceBase m, MonadBaseControl IO m)
  => ServerSettings (ResourceT m)
  -> NetApp m ()
  -> m ()
runServer s run =
  runResourceT $ CN.runTCPServer (networkServerSettings s) $ \ad -> do
    sd <- NS.toStreamData ad
    runReaderT (evalStateT (run >> close) (Left sd)) sd

runClient
  :: (MonadResourceBase m, MonadBaseControl IO m)
  => ClientSettings (ResourceT m)
  -> NetApp m ()
  -> m ()
runClient s run =
  runResourceT $ CN.runTCPClient (networkClientSettings s) $ \ad -> do
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
  :: (IsMessage msg, Monad m)
  => Conduit BL.ByteString m (Maybe msg)
toMsg = CL.map $ \bs ->
  case PB.messageGet bs of
       Right (m,r) | BL.null r -> Just m
       _                       -> Nothing

-- | Same as @condToMsg@ but ignore all @Nothing@s
toMsg'
  :: (IsMessage msg, Monad m)
  => Conduit BL.ByteString m msg
toMsg' = CL.mapMaybe $ \bs ->
  case PB.messageGet bs of
       Right (m,r) | BL.null r -> Just m
       _                       -> Nothing

fromMsg
  :: (IsMessage msg, Monad m)
  => Conduit msg m ByteString
fromMsg = CL.map (BL.toStrict . PB.messagePut)

getMsg
  :: (IsMessage msg, MonadResourceBase m)
  => NetApp m (Maybe msg)
getMsg = do
  r <- receive $ CL.isolate 1 =$ toMsg =$ CL.consume
  return $ case r of
    [Just x] -> Just x
    _        -> Nothing

sendMsg
  :: (IsMessage msg, MonadResourceBase m)
  => msg -> NetApp m ()
sendMsg msg = do
  send $ yield msg $= fromMsg

-- | Await a specific message. May fail with the `UnexpectedMessage` exception.
awaitMsg
  :: (IsMessage msg, MonadResourceBase m, Show msg)
  => NetApp m msg
awaitMsg = do
  msgs <- receive $ toMsg' =$= CL.consume
  case msgs of
       [msg] -> return msg
       _     -> throw $ UnexpectedMessage (map Msg msgs)

-- | Expect *exactly* this message. May fail with the `UnexpectedMessage`
-- exception.
expectMsg
  :: (Eq msg, IsMessage msg, MonadResourceBase m, Show msg)
  => msg
  -> NetApp m ()
expectMsg msg' = do
  msgs <- receive $ toMsg' =$= CL.consume
  case msgs of
       [msg] | msg == msg' -> return ()
       _                   -> throw $ UnexpectedMessage (map Msg msgs)

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
