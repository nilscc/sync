{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Sync.Types where

import Data.Word
import Crypto.RollingHash.Lookup

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Data.Conduit
import Data.Conduit.Network.Stream
import Data.ByteString (ByteString)

type BlockSize = Int
type FilePos   = Word64
type FileBlock = (FilePos, BlockSize)

--------------------------------------------------------------------------------
-- * Hashing types

class HasHashingMatch a where
  data HashingMatch a

instance HasHashingMatch RollingHash where
  data HashingMatch RollingHash = HashingMatchWeak
    { localFilePosWeak  :: FilePos
    , remoteFilePosWeak :: FilePos }

instance HasHashingMatch MD5 where
  data HashingMatch MD5 = HashingMatchStrong
    { localFilePosStrong :: FilePos }

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
