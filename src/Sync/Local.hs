{-# LANGUAGE ConstraintKinds #-}

module Sync.Local where

import Sync.Local.Hashing
import Sync.Types

compareLocalFile
  :: MonadResourceBase m
  => FilePath
  -> BlockSize
  -> NetApp m [HashingMatch MD5]
compareLocalFile fp blocksize = do
  sendRollingHashes fp blocksize
  getMatchingMD5s fp
