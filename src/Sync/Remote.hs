{-# LANGUAGE ConstraintKinds #-}

module Sync.Remote where

import Sync.Remote.Hashing
import Sync.Internal.Types
import Sync.Internal.Protocol
import Sync.Protocol

compareFileRemote
  :: MonadResourceBase m
  => NetApp m ()
compareFileRemote = do
  f <- awaitMsg
  sendMsg $ Reply OK
  lkup <- getRollingHashes f
  mtch <- findMatchingWeak f lkup
  sendMD5 f mtch
