{-# LANGUAGE ConstraintKinds #-}

module Sync.Local where

import Sync.Local.Hashing
import Sync.Types
import Sync.IO
import Sync.Protocol

compareLocalFile
  :: MonadResourceBase m
  => FilePath
  -> BlockSize
  -> NetApp m [HashingMatch MD5]
compareLocalFile fp blocksize = do
  f <- getFileInfo fp blocksize
  sendMsg f
  sendRollingHashes f
  getMatchingMD5s f
