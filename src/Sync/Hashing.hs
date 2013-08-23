{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sync.Hashing
  ( module Sync.Hashing.MD5
  , module Sync.Hashing.RollingBlocks
  ) where
  -- (
  --   getFileInfoP
  --   -- ** Client functions
  -- , clSendRollingHashes
  -- , clGetMatching, ClLookupMatched (..)
  -- , clUpload
  --   -- ** Server functions
  -- , srvGetRollingHash
  -- , srvSendMatching
  -- , srvGetMatched, SrvLookupMatched(..)
  -- , srvSaveUploadAs
  -- ) where

import Control.Monad
import Control.Monad.Trans
-- import Crypto.RollingHash
import Data.Conduit
import Data.Maybe
-- import Data.List
-- import Data.Word
import System.IO

-- import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Set (Set)
-- import qualified Crypto.Hash.MD5              as M5
-- import           Crypto.RollingHash.Lookup
-- import qualified Crypto.RollingHash.Internal  as RH
-- import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as BL
import qualified Data.Conduit.List            as CL
-- import qualified Data.Conduit.Binary          as CB
-- import qualified Data.Map.Strict              as M
-- import qualified Data.Sequence                as Seq
-- import qualified Data.Foldable                as F
-- import qualified Data.Set                     as S

import Sync.IO
-- import Sync.Exceptions
import Sync.Protocol
-- import Sync.Protocol.Internal
import Sync.Hashing.RollingBlocks
import Sync.Hashing.MD5
import Sync.Types

-- | Lookup map for remote/local file locations
type LookupLocations = Map FileLoc FileLoc

newtype SrvLookupMatched = SrvLookupMatched (Map FileLoc FileLoc)
newtype ClLookupMatched  = ClLookupMatched  (Set FileLoc)

getFileInfoP :: MonadIO m => FilePath -> BlockSize -> Source m FileTransferInfo
getFileInfoP fp bs = getFileInfo fp bs >>= yield

--------------------------------------------------------------------------------
-- Client

{-
clUpload :: MonadResourceBase m => Handle -> ClLookupMatched -> NetApp m ()
clUpload h (ClLookupMatched s) =
  send $ go 0
 where
  go :: MonadResource m => Int -> Source m (Int, BL.ByteString)
  go pos = do
    -- lookup next matched elem
    case S.lookupGE (FileLoc (fromIntegral pos) 0) s of

         Nothing -> do
          -- no more common blocks, send remaining bytes
          fs  <- liftIO $ hFileSize h
          let len = fromIntegral fs - pos -- Integer->Int hopefully won't crash
          when (len > 0) $ do
            blk <- liftIO $ do
              -- move handle first
              hSeek h AbsoluteSeek (fromIntegral pos)
              BL.hGet h len
            -- upload block
            yield (len, blk)

         Just (FileLoc p s_b)

          | fromIntegral p == pos ->
            -- skip block if currently on matched
            go (pos + fromIntegral s_b)

          | otherwise -> do
            -- upload block of size (p - pos)
            let new_pos = fromIntegral p
                len     = new_pos - pos
            blk <- liftIO $ do
              -- make sure the handle is at the correct position
              hSeek h AbsoluteSeek (fromIntegral pos)
              -- get block
              BL.hGet h len
            -- upload block
            yield (fromIntegral len,blk)
            -- continue looping
            go new_pos
-}

--------------------------------------------------------------------------------
-- Server

-- | Server: Get rolling hashes from the current stream
srvGetRollingHash
  :: MonadResourceBase m
  => FileTransferInfo
  -> NetApp m LookupWeak
srvGetRollingHash fi = do
  let s = fromIntegral $ ft_blocksize fi
  rhshs <- receiveList $ CL.map (\(f :: BlockSize -> RollingHash) -> f s)
  return $ toLookupWeak s rhshs

-- | Server: Put (remote) file locations of (weakly) matched blocks into the
-- current stream and return the lookup map for strong hashes of those blocks
srvFindMatchingWeak
  :: MonadResourceBase m
  => FilePath
  -> LookupWeak
  -> NetApp m [HashingMatch RollingHash]
srvFindMatchingWeak fp lkup = do
  bs <- liftIO $ BL.readFile fp
  let blocksize = lookup_blocksize lkup
      blocks    = toRollingBlocks blocksize bs
      matching  = findMatching lkup blocks
  return matching

srvSendMD5
  :: MonadResourceBase m
  => FilePath
  -> BlockSize
  -> [HashingMatch RollingHash]
  -> NetApp m ()
srvSendMD5 fp blocksize matches = do
  h <- withBinaryFile' fp ReadMode
  send $ forM_ matches $ \match -> do
    md5 <- hashBlockMD5 h blocksize match
    yield md5 $= fromMsg

{-
srvGetMD5Matches
  :: MonadResourceBase m
  => LookupLocations
  -> NetApp m SrvLookupMatched
srvGetMD5Matches m =
  receive $ toMsg' =$ srvRcvUnmatched m
-}

{-
-- | Server: Receive unmatched file positions, returns a list of local blocks to
-- send upstream
srvRcvUnmatched :: Monad m => LookupLocations -> Sink FileLoc m SrvLookupMatched
srvRcvUnmatched m = do
  -- delete any unmatched blocks
  SrvLookupMatched `liftM` CL.fold (\m' l -> M.delete l m') m

srvSaveUploadAs
  :: MonadResourceBase m
  => FilePath               -- ^ Location of the local file
  -> SrvLookupMatched
  -> FilePath               -- ^ Location for the uploaded file
  -> NetApp m ()
srvSaveUploadAs fp_loc (SrvLookupMatched m) fp_up = do
  h <- withBinaryFile' fp_loc ReadMode
  receive $ go h 0 =$ CB.sinkFile fp_up
 where
  go :: MonadResource m
     => Handle
     -> Int -- ^ Position in the remote (client) file!
     -> Conduit BL.ByteString m ByteString
  go h pos =
    -- lookup next matched elem
    case M.lookupGE (FileLoc (fromIntegral pos) 0) m of

         Nothing ->
          -- no more common blocks, store remaining incoming bytes
          awaitForever $ \bs ->
            mapM_ yield (BL.toChunks bs)

         Just (FileLoc p_r s_br, FileLoc p_l s_bl)

          -- copy block from local file:
          | fromIntegral p_r == pos -> do
            -- move handle
            liftIO $ hSeek h AbsoluteSeek (fromIntegral p_l)
            -- read @s_bl@ number of bytes and pipe them into our file
            hGetPipe h s_bl
            -- continue from position p_r + s_br
            go h (fromIntegral p_r + fromIntegral s_br)

          -- pipe incoming block to file
          | otherwise -> do
            mbs <- await
            case mbs of
                 Nothing -> monadThrow UnexpectedEndOfInput
                 Just bs -> do
                  mapM_ yield $ BL.toChunks bs
                  -- set pos to p_r => copy next block
                  go h (fromIntegral p_r)

  hGetPipe h s_bl = do
    let s = minimum [s_bl, 4096]
        r = s_bl - s
    blk <- liftIO $ BS.hGet h (fromIntegral s)
    yield blk
    when (r > 0) $
      hGetPipe h r
-}
