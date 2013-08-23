{-# LANGUAGE ConstraintKinds #-}

module Sync.Local where

import Sync.Local.Hashing
import Sync.Internal.Types
import Sync.Internal.IO
import Sync.Internal.Protocol
import Sync.Protocol

compareLocalFile
  :: MonadResourceBase m
  => FilePath
  -> BlockSize
  -> NetApp m [HashingMatch MD5]
compareLocalFile fp blocksize = do
  f <- getFileTransferInfo fp blocksize
  expectMsg $ Reply OK
  sendMsg f
  sendRollingHashes f
  getMatchingMD5s f
