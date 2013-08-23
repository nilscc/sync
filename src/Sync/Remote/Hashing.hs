{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}

module Sync.Remote.Hashing where

import           Control.Monad
import           Control.Monad.Trans
import qualified Data.ByteString.Lazy         as BL
import           Data.Conduit
import qualified Data.Conduit.List            as CL
import           System.IO
import           Text.ProtocolBuffers

import           Sync.Internal.IO
import           Sync.Internal.Protocol
import           Sync.Internal.Types
import           Sync.Hashing

getFileTranssferInfo
  :: MonadResourceBase m
  => NetApp m (Maybe FileTransferInfo)
getFileTranssferInfo = getMsg

-- | Server: Get rolling hashes from the current stream
getRollingHashes
  :: MonadResourceBase m
  => FileTransferInfo
  -> NetApp m LookupWeak
getRollingHashes fi = do
  let s = fromIntegral $ ft_blocksize fi
  rhshs <- receiveList $ CL.map (\(f :: BlockSize -> RollingHash) -> f s)
  return $ toLookupWeak s rhshs

-- | Server: Put (remote) file locations of (weakly) matched blocks into the
-- current stream and return the lookup map for strong hashes of those blocks
findMatchingWeak
  :: MonadResourceBase m
  => FileTransferInfo
  -> LookupWeak
  -> NetApp m [HashingMatch RollingHash]
findMatchingWeak fi lkup = do
  bs <- liftIO $ BL.readFile fp
  let blocksize = lookup_blocksize lkup
      blocks    = toRollingBlocks blocksize bs
      matching  = findMatching lkup blocks
  return matching
 where
  fp = toString $ ft_filename fi

sendMD5
  :: MonadResourceBase m
  => FileTransferInfo
  -> [HashingMatch RollingHash]
  -> NetApp m ()
sendMD5 fi matches = do
  h <- withBinaryFile' fp ReadMode
  send $ forM_ matches $ \match -> do
    md5 <- hashBlockMD5 h blocksize match
    yield md5 $= fromMsg
 where
  fp        = toString     $ ft_filename fi
  blocksize = fromIntegral $ ft_blocksize fi
