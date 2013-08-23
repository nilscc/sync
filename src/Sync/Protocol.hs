{-# LANGUAGE ConstraintKinds #-}

module Sync.Protocol
  ( -- * Binary network protocol
    runServer, runClient
  , serverSettings, ServerSettings, CN.HostPreference (..)
  , clientSettings, ClientSettings
    -- * Protocol buffer messages
  , module Sync.Protocol.ProtoBuff
  , module Sync.Protocol.ProtoBuff.Command
  , module Sync.Protocol.ProtoBuff.Command.Cmd
  , module Sync.Protocol.ProtoBuff.Reply
  , module Sync.Protocol.ProtoBuff.Reply.Rply
  , module Sync.Protocol.ProtoBuff.FileTransferInfo
  , module Sync.Protocol.ProtoBuff.FileLoc
  , module Sync.Protocol.ProtoBuff.MD5Hash
  ) where

import Sync.Internal.Protocol
import Sync.Internal.Types

import qualified Data.Conduit.Network as CN

-- re-exports
import Sync.Protocol.ProtoBuff
import Sync.Protocol.ProtoBuff.Command
import Sync.Protocol.ProtoBuff.Command.Cmd
import Sync.Protocol.ProtoBuff.Reply
import Sync.Protocol.ProtoBuff.Reply.Rply
import Sync.Protocol.ProtoBuff.FileTransferInfo
import Sync.Protocol.ProtoBuff.FileLoc
import Sync.Protocol.ProtoBuff.MD5Hash

serverSettings
  :: Monad m
  => FilePath     -- ^ Base directory
  -> Port
  -> CN.HostPreference
  -> ServerSettings m
serverSettings dir p h = ServerSettings
  { networkServerSettings = CN.serverSettings p h
  , serverBaseDir         = dir }

clientSettings
  :: Monad m
  => FilePath     -- ^ Base directory
  -> Port
  -> Host
  -> ClientSettings m
clientSettings dir p h = ClientSettings
  { networkClientSettings = CN.clientSettings p h
  , clientBaseDir         = dir }
