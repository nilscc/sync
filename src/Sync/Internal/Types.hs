{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Sync.Internal.Types where

import           Data.Word
import           Crypto.RollingHash.Lookup

import           Control.Applicative
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Conduit
import qualified Data.Conduit.Network         as CN
import           Data.Conduit.Network.Stream
import           Data.ByteString (ByteString)
import           Text.ProtocolBuffers

type BlockSize = Int
type FilePos   = Word64
type FileBlock = (FilePos, BlockSize)

type Host = ByteString
type Port = Int

type IsMessage msg = (ReflectDescriptor msg, Wire msg)
--------------------------------------------------------------------------------
-- * Settings

data ClientSettings m = ClientSettings
  { networkClientSettings :: CN.ClientSettings m
  , clientBaseDir         :: FilePath
  }

data ServerSettings m = ServerSettings
  { networkServerSettings :: CN.ServerSettings m
  , serverBaseDir         :: FilePath
  }

--------------------------------------------------------------------------------
-- * Hashing types

class HasHashingMatch a where
  data HashingMatch a

instance HasHashingMatch RollingHash where
  data HashingMatch RollingHash = HashingMatchWeak
    { localFilePosWeak  :: FilePos
    , remoteFilePosWeak :: FilePos }
    deriving Show

instance HasHashingMatch MD5 where
  data HashingMatch MD5 = HashingMatchStrong
    { localFilePosStrong :: FilePos }
    deriving Show

type RollingHash = (FilePos, Word32)

-- | Lookup map for weak (rolling) hashes
data LookupWeak = LookupWeak
  { lookup_map       :: Word32Map FilePos
  , lookup_blocksize :: BlockSize
  }

type MD5 = ByteString

--------------------------------------------------------------------------------
-- * Application types

type NetApp    m = StateT  (NetState  m) (ReaderT         (NetData m)   (ResourceT m))
type NetStream m = ResourceT m
type NetData   m = StreamData (NetStream m)
type NetState  m = Either  (NetData   m) (ResumableSource (NetStream m) (ByteString))

type MonadResourceBase m = (MonadIO m, MonadUnsafeIO m, MonadThrow m, Applicative m)
type NetSendable   a m   = Sendable (ResourceT m) a
type NetReceivable a m   = Receivable a (ResourceT m)
