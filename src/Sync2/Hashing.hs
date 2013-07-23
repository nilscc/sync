{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE BangPatterns #-}

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
import Crypto.RollingHash
import Data.Conduit
import Data.List
import Data.Word
import System.IO

import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Set (Set)
import qualified Crypto.Hash.MD5              as M5
import           Crypto.RollingHash.Lookup
import qualified Crypto.RollingHash.Internal  as RH
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as BL
import qualified Data.Conduit.List            as CL
import qualified Data.Conduit.Binary          as CB
import qualified Data.ByteString.Lazy.Builder as BUILDER
import qualified Data.Map.Strict              as M
import qualified Data.Sequence                as Seq
import qualified Data.Foldable                as F
import qualified Data.Set                     as S

import Sync2.IO
import Sync2.Exceptions
import Sync2.Protocol
import Sync2.Protocol.Internal

-- | Lookup map for weak (rolling) hashes
type LookupWeak = Word32Map Int

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
  bs <- liftIO $ BL.readFile fp
  send $ go (mkR s bs) $= w32toBS
 where
  go !r = case isEmptyR r of
    True  -> return ()
    False -> yield (hashR32 r) >> go (rollBlock r)

{-
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
-}

w32toBS :: Monad m => Conduit Word32 m ByteString
w32toBS = CL.map $ \w32 ->
  BL.toStrict $ BUILDER.toLazyByteString (BUILDER.word32LE w32)

-- | Client: Compare local file with incoming hashes, send out unmatched file
-- locations and return matched file locations as lookup list
clGetMatching :: MonadResourceBase m => Handle -> NetApp m ClLookupMatched
clGetMatching h = do
  sd <- getStreamData
  l  <- receive $ toMsg' =$ clHashMatching sd h
  return $ ClLookupMatched $ S.fromList l

-- | Client: Compare local file with incoming hashes and send out unmatched(!)
-- file locations, return matched(!) file locations
clHashMatching
  :: MonadResourceBase m
  => NetData m
  -> Handle
  -> Sink MD5Hash (NetStream m) [FileLoc]
clHashMatching sd h = do
  -- start outgoing list
  withElementSink sd $
    go []
 where
  go ms sink = do
    a <- await
    case a of
         Nothing                   -> return ms -- done
         Just (MD5Hash hsh loc) -> do
           let p = fromIntegral $ fl_position loc
               s = fromIntegral $ fl_size     loc
           -- hash local block
           block <- liftIO $ do
             hSeek h AbsoluteSeek p
             BL.hGet h s
           let hsh = (loc, BL.fromStrict (M5.hashlazy block))
           -- split matched & unmatched blocks
           let (mtchd,umtchd) =
                 foldr ( \(loc,hsh') (m,u) ->
                         if hsh == hsh'
                            then (loc:m, u)  -- add matches to matched 'm' list
                            else (m, loc:u)) -- add unmatched to 'u' list
                       ([],[]) hsh
           -- send out unmatched file locations
           () <- mapM_ yield umtchd =$ fromMsg =$ sink
           -- loop
           go (ms ++ mtchd) sink

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

--------------------------------------------------------------------------------
-- Server

-- | Server: Get rolling hashes from the current stream
srvGetRollingHash
  :: MonadResourceBase m
  => FileTransferInfo
  -> NetApp m LookupWeak
srvGetRollingHash fi = {-# SCC srvGetRollingHash #-} receive $ srvRcvRollingHash fi

-- | Server: Receive rolling hashes from the current stream
srvRcvRollingHash
  :: MonadResource m
  => FileTransferInfo
  -> Sink BL.ByteString m LookupWeak
srvRcvRollingHash fi =
  w32s =$ to_lookup
 where
  s = fromIntegral $ ft_blocksize fi

  w32s :: Monad m => Conduit BL.ByteString m Word32
  w32s = CL.map $ \(!bs) ->
    case BL.unpack bs of
         [a,b,c,d] -> RH.buildWord32 a b c d
         _         -> error "Received invalid Word32"

  to_lookup :: MonadResource m => Sink Word32 m LookupWeak
  to_lookup = do
    w32_l <- CL.consume
    return $ fromListW32 $ zip w32_l [0,s..]

-- | Server: Put (remote) file locations of (weakly) matched blocks into the
-- current stream and return the lookup map for strong hashes of those blocks
srvSendMatching
  :: MonadResourceBase m
  => FilePath
  -> LookupWeak
  -> BlockSize
  -> NetApp m LookupLocations
srvSendMatching fp m s_blk = do
  sd <- getStreamData
  send $ srvPutMatching fp m s_blk $= (go `flip` M.empty)
 where
  go sink !m' = do
    r <- await
    case r of
         Nothing         -> return m'
         Just (md5, loc) -> do
           () <- yield md5 =$ fromMsg =$ sink
           -- add location to lookup map
           go sink $ M.insert (md5_loc md5) loc m'

-- | Server: Put MD5 hash and remote file location of (weakly) matched blocks
-- into the current stream
srvPutMatching
  :: MonadResource m
  => FilePath
  -> LookupWeak
  -> BlockSize
  -> Source m (MD5Hash, FileLoc)
srvPutMatching fp m s_blk = do
  h  <- withBinaryFile' fp ReadMode
  bs <- liftIO $ BL.hGetContents h
  startrollin 0 $ mkR s_blk bs
 where
  startrollin !p !r
    | isEmptyR r = return () -- done rollin'
    | otherwise  = 
      case lookupW32 (hashR32 r) m of
           Nothing    -> startrollin (p+1) (roll r)
           Just blkid -> do
            let ctx = hash r M5.init
            match blkid p ctx (p+s_blk) (rollBlock r)

  match blkid p_start !ctx !p !r 
    | isEmptyR r = done ctx blkid p_start p
    | otherwise  =
      case lookupW32 (hashR32 r) m of

           Nothing -> do

             done ctx blkid p_start p
             -- continue
             startrollin (p+1) (roll r)

           Just _ -> do

             let ctx' = hash r ctx
             match blkid p_start ctx' (p+s_blk) (rollBlock r)

  hash r ctx =
    let bs = BL.take (fromIntegral s_blk) (RH.dat_0 r)
     in foldl' (\ctx' chunk -> M5.update ctx' chunk) ctx (BL.toChunks bs)

  -- send MD5 hash upstream
  done ctx blkid p_start p =
    let blk_len = p - p_start
        -- local file location
        fl_l    = FileLoc (fromIntegral p_start) (fromIntegral blk_len)
        -- remote file location
        p_r     = blkid * s_blk
        fl_r    = FileLoc (fromIntegral p_r)     (fromIntegral blk_len)
        
     in yield $ ( MD5Hash (BL.fromStrict (M5.finalize ctx)) fl_r
                , fl_l )

{-
      case map (\c -> FileLoc (fromIntegral c * fromIntegral s_blk) (fromIntegral s_blk)) cs of { !fl ->
             -- on match: get MD4 hash of current block
             case FileLoc (fromIntegral p) (fromIntegral s_blk) of { !loc ->
             -- take block bytestring from 'r'
             case BL.take (fromIntegral s_blk) (RH.dat_0 r)     of { !blk ->
             case M4.hashlazy blk                               of { !hsh -> do
               -- send hash + remote location upstream
               yield (hsh, loc, fl)
               -- continue rollin' with the next block
               startrollin (p+s_blk) (rollBlock r)
             }}}}
-}

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
