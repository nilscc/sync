{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Sync.Internal.Exceptions where

import Control.Exception
import Data.Typeable

import Sync.Internal.Types

data SyncException
  = UnexpectedEndOfInput
  | UnexpectedMessage Msg
  deriving (Typeable, Show)

instance Exception SyncException

data Msg where
  Msg :: (Show msg, IsMessage msg) => msg -> Msg

instance Show Msg where
  show (Msg m) = show m
