{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Sync.IO
  ( BlockSize
  , getFileSize
  , getFileInfo
  , withBinaryFile', withBinaryTempFile'
  , CB.sourceFile
  ) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import System.Directory
import System.IO
import Text.ProtocolBuffers

import qualified Data.Conduit.Binary as CB

import Sync.Protocol

--------------------------------------------------------------------------------
-- IO

type BlockSize = Int

getFileInfo :: MonadIO m => FilePath -> BlockSize -> m FileTransferInfo
getFileInfo fp bs = do
  s <- getFileSize fp
  return $ FileTransferInfo (fromString fp) (fromIntegral s) (fromIntegral bs)

getFileSize :: MonadIO m => FilePath -> m Integer
getFileSize fp = liftIO $ withBinaryFile fp ReadMode hFileSize

withBinaryFile'
  :: MonadResource m => FilePath -> IOMode -> m Handle
withBinaryFile' fp iom =
  snd `liftM` allocate (openBinaryFile fp iom)
                       (hClose)

withBinaryTempFile'
  :: MonadResource m
  => FilePath
  -> String
  -> Bool -- ^ delete file when done?
  -> m (FilePath, Handle)
withBinaryTempFile' fp template delete = snd `liftM` allocate
  (openBinaryTempFile fp template)
  (\(fp',h') -> do
    hClose h'
    when delete (removeFile fp'))