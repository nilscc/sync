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

import Sync.Hashing.MD5
import Sync.Hashing.RollingBlocks

{-
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
-}
