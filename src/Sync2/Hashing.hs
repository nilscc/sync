{-# LANGUAGE ConstraintKinds #-}

module Sync2.Hashing
  (
    getFileInfoP
    -- ** Client functions
  , clSendRollingHashes
  , clGetMatching, ClLookupMatched (..)
  , clUpload
    -- ** Server functions
  , srvGetRollingHash
  , srvSendMatching
  , srvGetMatched, SrvLookupMatched(..)
  , srvSaveUploadAs
  ) where

import Control.Monad
import Control.Monad.Trans
import Data.Conduit
--import Data.List
import Data.Word
import System.IO

import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Set (Set)
import qualified Crypto.Hash.MD4              as M4
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as BL
import qualified Data.Conduit.List            as CL
import qualified Data.Conduit.Binary          as CB
import qualified Data.ByteString.Lazy.Builder as BUILDER
import qualified Data.Map                     as M
import qualified Data.Sequence                as Seq
import qualified Data.Foldable                as F
import qualified Data.Set                     as S

import Sync2.IO
import Sync2.Exceptions
import Sync2.Protocol
import Sync2.Protocol.Internal
import Sync2.Hashing.RollingHash

-- | Lookup map for weak (rolling) hashes
type LookupWeak = Map Word32 [FileLoc]

-- | Lookup map for remote/local file locations
type LookupLocations = Map FileLoc FileLoc

newtype SrvLookupMatched = SrvLookupMatched (Map FileLoc FileLoc)
newtype ClLookupMatched  = ClLookupMatched  (Set FileLoc)

getFileInfoP :: MonadIO m => FilePath -> BlockSize -> Source m FileTransferInfo
getFileInfoP fp bs = getFileInfo fp bs >>= yield

--------------------------------------------------------------------------------
-- Client

-- | Client: Get weak (\"rolling\") hashes for each block and send them over the stream
clSendRollingHashes :: MonadResourceBase m => FilePath -> BlockSize -> NetApp m ()
clSendRollingHashes fp s = do
  sendList $ sourceFile fp $= clPutRollingHash s

clPutRollingHash :: Monad m => BlockSize -> Conduit ByteString m ByteString
clPutRollingHash s = loop =$= w32toBS
 where
  loop = do
    b <- CB.take s
    if BL.null b then
       return ()
     else do
       let l = BL.length b
       yield $ hash (mkR 0 (fromIntegral l) (fromIntegral l) b)
       loop

w32toBS :: Monad m => Conduit Word32 m ByteString
w32toBS = CL.map $ \w32 ->
  BL.toStrict $ BUILDER.toLazyByteString (BUILDER.word32LE w32)

-- | Client: Compare local file with incoming hashes, send out unmatched file
-- locations and return matched file locations as lookup list
clGetMatching :: MonadResourceBase m => Handle -> NetApp m ClLookupMatched
clGetMatching h = do
  ad <- getAppData
  l  <- receive $ toMsg' =$ clHashMatching ad h
  return $ ClLookupMatched $ S.fromList l

-- | Client: Compare local file with incoming hashes and send out unmatched(!)
-- file locations, return matched(!) file locations
clHashMatching
  :: MonadResourceBase m
  => NetData m
  -> Handle
  -> Sink MD4Hash (NetStream m) [FileLoc]
clHashMatching ad h = do
  -- start outgoing list
  sinkList' ad $
    go []
 where
  go ms sink = do
    a <- await
    case a of
         Nothing                   -> return ms -- done
         Just (MD4Hash hsh locs_s) -> do
           let locs = F.toList locs_s
           hsh_l <- forM locs $ \loc -> do
             let p = fromIntegral $ fl_position loc
                 s = fromIntegral $ fl_size     loc
             -- hash local block
             block <- liftIO $ do
               hSeek h AbsoluteSeek p
               BL.hGet h s
             return $ (loc, BL.fromStrict (M4.hashlazy block))
           -- split matched & unmatched blocks
           let (mtchd,umtchd) =
                 foldr ( \(loc,hsh') (m,u) ->
                         if hsh == hsh'
                            then (loc:m, u)  -- add matches to matched 'm' list
                            else (m, loc:u)) -- add unmatched to 'u' list
                       ([],[]) hsh_l
           -- send out unmatched file locations
           () <- mapM_ yield umtchd =$ fromMsg =$ sink
           -- loop
           go (ms ++ mtchd) sink

clUpload :: MonadResourceBase m => Handle -> ClLookupMatched -> NetApp m ()
clUpload h (ClLookupMatched s) =
  sendList $ go 0
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

--------------------------------------------------------------------------------
-- Server

-- | Server: Get rolling hashes from the current stream
srvGetRollingHash
  :: MonadResourceBase m
  => FileTransferInfo
  -> NetApp m LookupWeak
srvGetRollingHash fi = receive $ srvRcvRollingHash fi

-- | Server: Receive rolling hashes from the current stream
srvRcvRollingHash
  :: Monad m
  => FileTransferInfo
  -> Sink BL.ByteString m LookupWeak
srvRcvRollingHash fi =
  w32s =$ fmap snd to_lookup
 where
  s = fromIntegral $ ft_blocksize fi

  w32s :: Monad m => Conduit BL.ByteString m Word32
  w32s = CL.mapMaybe $ \bs ->
    case BL.unpack bs of
         [a,b,c,d] -> Just $ buildWord32 a b c d
         _         -> Nothing

  to_lookup :: Monad m => Sink Word32 m (Word64, LookupWeak)
  to_lookup = CL.fold `flip` (0, M.empty) $ \(c,m) w32 ->
    -- TODO / FIXME: Last blocksize might be less than 's'. This should usually
    -- not be an issue because it's only being used by 'clHashMatching' with
    -- 'BL.hGet' which terminates if the size is too long.
    let fl = FileLoc (c * s) (fromIntegral s)
        m' = M.alter (add_fl fl) w32 m
     in (c+1, m')

  add_fl fl (Nothing) = Just [fl]
  add_fl fl (Just l)  = Just (fl:l)

-- | Server: Put (remote) file locations of (weakly) matched blocks into the
-- current stream and return the lookup map for strong hashes of those blocks
srvSendMatching
  :: MonadResourceBase m
  => FilePath
  -> LookupWeak
  -> BlockSize
  -> NetApp m LookupLocations
srvSendMatching fp m s_blk = do
  ad <- getAppData
  srvPutMatching fp m s_blk ~~ sinkList' ad (go `flip ` M.empty)
 where
  go sink m' = do
    r <- await
    case r of
         Nothing -> return m'
         Just (hsh, loc, rems) -> do
           -- send remote file locations of matches upstream
           let md4 = MD4Hash (BL.fromStrict hsh) (Seq.fromList rems)
           () <- yield md4 =$ fromMsg =$ sink
           -- add md4 hash to lookup map
           go sink $ foldr (M.insert `flip` loc) m' rems

-- | Server: Put MD4 hash and remote file locations of (weakly) matched blocks
-- into the current stream
srvPutMatching
  :: MonadResource m
  => FilePath
  -> LookupWeak
  -> BlockSize
  -> Source m (ByteString, FileLoc, [FileLoc])
srvPutMatching fp m s_blk = do
  h  <- withBinaryFile' fp ReadMode
  bs <- liftIO $ BL.hGetContents h
  fs <- liftIO $ hFileSize h
  startrollin $ mkR 0 s_blk fs bs
 where
  startrollin r
    | isEmptyR r = return () -- done rollin'
    | otherwise  =
      case M.lookup (hash r) m of
           Nothing -> startrollin (roll r)
           Just fl -> do
             -- on match: get MD4 hash of current block
             let loc = rFileLocation r
                 blk = rGetBlock r
                 hsh = M4.hashlazy $ BL.pack blk
             -- send hash + remote location upstream
             yield (hsh, loc, fl)
             -- continue rollin' with the next block
             startrollin (skipBlock r)

srvGetMatched :: MonadResourceBase m => LookupLocations -> NetApp m SrvLookupMatched
srvGetMatched m = receive $ toMsg' =$ srvRcvUnmatched m

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
