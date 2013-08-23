{-# LANGUAGE ConstraintKinds #-}

module Sync.Remote where

import Sync.Remote.Hashing
import Sync.Internal.Types

compareFileRemote
  :: MonadResourceBase m
  => NetApp m ()
compareFileRemote = do
  mf <- getFileTranssferInfo
  case mf of
       Nothing -> return ()
       Just f  -> do
         lkup <- getRollingHashes f
         mtch <- findMatchingWeak f lkup
         sendMD5 f mtch
