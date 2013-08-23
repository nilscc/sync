{-# LANGUAGE DeriveDataTypeable #-}

module Sync.Exceptions where

import Control.Exception
import Data.Typeable

data SyncException
  = UnexpectedEndOfInput
  deriving (Typeable, Show)

instance Exception SyncException
