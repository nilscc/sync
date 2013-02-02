{-# LANGUAGE DeriveDataTypeable #-}

module Sync2.Exceptions where

import Control.Exception
import Data.Typeable

data Sync2Exception
  = UnexpectedEndOfInput
  deriving (Typeable, Show)

instance Exception Sync2Exception
