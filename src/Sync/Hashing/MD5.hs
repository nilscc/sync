module Sync.Hashing.MD5
  ( hashBlockMD5, compareMD5
  ) where

import           Control.Monad.Trans
-- import           Data.ByteString
import qualified Crypto.Hash.MD5          as MD5
import qualified Data.ByteString.Lazy     as BL
import           System.IO

import Sync.Types
import Sync.Protocol

hashBlockMD5
  :: MonadIO m
  => Handle                         -- ^ handle to (open) file
  -> BlockSize
  -> HashingMatch RollingHash
  -> m MD5Hash
hashBlockMD5 h blocksize match = do
  hsh <- hashBlock h (localFilePosWeak match) blocksize
  let fileloc = FileLoc { fl_position = fromIntegral $ remoteFilePosWeak match
                        , fl_size     = fromIntegral blocksize }
      md5     = MD5Hash { md5_hash = hsh
                        , md5_loc  = fileloc }
  return md5

compareMD5
  :: MonadIO m
  => Handle
  -> MD5Hash
  -> m (Maybe (HashingMatch MD5))
compareMD5 h md5 = do
  let loc       = md5_loc md5
      pos       = fromIntegral $ fl_position loc
      blocksize = fromIntegral $ fl_size loc
  hsh <- hashBlock h pos blocksize
  return $ if hsh == md5_hash md5
              then Just HashingMatchStrong{ localFilePosStrong = pos }
              else Nothing

-- Internal only
hashBlock
  :: MonadIO m
  => Handle
  -> FilePos
  -> BlockSize
  -> m BL.ByteString
hashBlock h pos blksize = liftIO $ do
  hSeek h AbsoluteSeek (fromIntegral pos)
  bs <- BL.hGet h (fromIntegral blksize)
  return $ BL.fromStrict $ MD5.hashlazy bs
