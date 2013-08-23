{-# LANGUAGE ConstraintKinds #-}

module Sync.Local.Hashing where

import           Control.Monad.Trans
import qualified Data.ByteString.Lazy         as BL
import           Data.Maybe
import           System.IO
import           Text.ProtocolBuffers

import           Sync.Hashing
import           Sync.Internal.Types
import           Sync.Internal.IO
import           Sync.Internal.Protocol

-- | Get weak (\"rolling\") hashes for each block and send them over the stream
sendRollingHashes
  :: MonadResourceBase m
  => FileTransferInfo
  -> NetApp m ()
sendRollingHashes fi = do
  bs <- liftIO $ BL.readFile fp
  sendList $ toRollingBlocks s bs
 where
  fp = toString     $ ft_filename fi
  s  = fromIntegral $ ft_blocksize fi

-- | Compare local file with incoming hashes, send out unmatched file
-- locations and return matched file locations as lookup list
getMatchingMD5s
  :: MonadResourceBase m
  => FileTransferInfo
  -> NetApp m [HashingMatch MD5]
getMatchingMD5s fi = do
  h       <- withBinaryFile' fp ReadMode
  md5s    <- receiveList $ toMsg'
  matches <- mapM (compareMD5 h) md5s
  return $ catMaybes matches
 where
  fp = toString $ ft_filename fi
