module Sync2.Hashing
  ( getFileInfoP
  , getRollingHash, putRollingHash
  , putMatching, hashMatching, checkMatching, getMatching
  , sendUnmatched, receiveUnmatched
  ) where

import Control.Monad
import Control.Monad.Trans
import Data.Conduit
import Data.List
import Data.Word
import System.IO

import Data.ByteString (ByteString)
import Data.Map (Map)
import qualified Crypto.Hash.MD4              as M4
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as BL
import qualified Data.Conduit.List            as CL
import qualified Data.Conduit.Binary          as CB
import qualified Data.ByteString.Lazy.Builder as BUILDER
import qualified Data.Map                     as M

import Sync2.IO
import Sync2.Protocol
import Sync2.Protocol.Internal
import Sync2.Protocol.VarInt
import Sync2.Hashing.RollingHash

import Debug.Trace

getFileInfoP :: MonadIO m => FilePath -> BlockSize -> Source m FileTransferInfo
getFileInfoP fp bs = getFileInfo fp bs >>= yield

putRollingHash :: Monad m => BlockSize -> Conduit ByteString m ByteString
putRollingHash s = condEncRolling s =$= w32toBS

condEncRolling :: Monad m => BlockSize -> Conduit ByteString m Word32
condEncRolling s =
  loop
 where
  loop = do
    b <- CB.take s
    if BL.null b then
       return ()
     else do
       yield $ hash (mkR 0 (fromIntegral $ BL.length b) b)
       loop

w32toBS :: Monad m => Conduit Word32 m ByteString
w32toBS = CL.map $ \w32 ->
  BL.toStrict $ BUILDER.toLazyByteString (BUILDER.word32LE w32)

type HashLookup = Map Word32 FileLoc

getRollingHash
  :: Monad m
  => FileTransferInfo
  -> Sink BL.ByteString m HashLookup
getRollingHash fi =
  w32s =$ fmap snd to_lookup
 where
  s = fromIntegral $ ft_blocksize fi

  w32s :: Monad m => Conduit BL.ByteString m Word32
  w32s = CL.mapMaybe $ \bs ->
    case BL.unpack bs of
         [a,b,c,d] -> Just $ buildWord32 a b c d
         _         -> Nothing

  to_lookup :: Monad m => Sink Word32 m (Word64, HashLookup)
  to_lookup = CL.fold `flip` (0, M.empty) $ \(c,m) w32 ->
    let fl = FileLoc (c * s) (fromIntegral s)
     in trace ("to_lookup, " ++ show (c+1)) (c+1, M.insert w32 fl m)

putMatching
  :: MonadResource m
  => Handle -> HashLookup -> BlockSize -> Source m FileLoc
putMatching h m s_blk = do
  bs <- liftIO $ BL.hGetContents h
  startrollin $ mkR 0 s_blk bs
 where
  startrollin r
    | emptyR r  = return ()
    | otherwise =
      case M.lookup (hash r) m of
           Just fl -> yield fl
                   >> startrollin (skipBlock r)
           Nothing -> startrollin (roll r)

-- | Client: Hash incoming file locations and send the MD4 hashes to the server
hashMatching :: MonadResource m => Handle -> Conduit FileLoc m MD4Hash
hashMatching h = do
  -- handle incoming matches
  awaitForever $ \m -> do
    let p = fromIntegral $ fl_position m
        s = fromIntegral $ fl_size m
    -- hash local block
    block <- liftIO $ do
      hSeek h AbsoluteSeek p
      BL.hGet h s
    -- send hash to server
    yield $ MD4Hash m (BL.fromStrict $ M4.hashlazy block)

-- | Server: Check incoming MD4 hashes and send file locations of matched blocks
checkMatching :: MonadIO m => Handle -> Conduit MD4Hash m FileLoc
checkMatching h = do
  awaitForever $ \m -> do
    let l = md4_loc m
        p = fromIntegral $ fl_position l
        s = fromIntegral $ fl_size l
    -- hash local block
    block <- liftIO $ do
      hSeek h AbsoluteSeek p
      BL.hGet h s
    let hsh = BL.fromStrict $ M4.hashlazy block
    -- send file location to client if hashes match
    when (hsh == md4_hash m) $
      yield l

-- | Client: Receive matched blocks
getMatching :: Monad m => Sink BL.ByteString m [FileLoc]
getMatching = condToMsg' =$ CL.consume

-- | Server: Send unmatched file blocks
sendUnmatched :: MonadIO m => Handle -> [FileLoc] -> NetApp m ()
sendUnmatched h matches_unsorted = do
  -- make sure our matches are sorted (this should usually be the case)
  let matches = sort matches_unsorted
  sendBlocks matches $$ sendSizedBS

  return ()
 where
  sendBlocks matches = do
    -- fold over all matches, start sending bytes from 0
    _ <- foldM (\cur (FileLoc p s) -> go cur (fromIntegral p) (fromIntegral s))
               0 matches
    return ()

  go cur p s | cur <= p  = sendBlock (p - cur) >> return (p+s)
             | otherwise = return (p+s)

  sendBlock l = do
    -- send block size
    yield $ SizedBS (BS.pack (varint l))
    -- send data
    loop l

  loop l = do
    -- split our strict bytestring into "lazy" blocks
    let s_bsblk = 4096
        rest    = l - s_bsblk
    blk <- liftIO $ BS.hGet h (minimum [s_bsblk, l])
    yield $ SizedBS blk
    -- send rest
    when (rest > 0) $
      loop l

receiveUnmatched
  :: MonadIO m
  => Handle       -- ^ open (local) file
  -> Handle       -- ^ temporary file
  -> [FileLoc]
  -> Sink BL.ByteString m ()
receiveUnmatched h_loc h_tmp matches_unsorted = do
  -- again, just to make sure:
  let matches = sort matches_unsorted
  -- store incoming bytestrings and copy matched ones
  mapM_ (\(FileLoc p s) -> go (fromIntegral p) (fromIntegral s)) matches
  -- when there are no matches left, store the rest of the incoming data
  store

 where

  go p s = do
    -- get current position
    p' <- liftIO $ hTell h_loc
    if p == p' then
       -- if we're at a matching location, don't wait for incoming data
       copy s
     else do
       -- otherwise, store incoming data first
       store
       -- then move handle to position & start copying
       liftIO $ hSeek h_loc AbsoluteSeek p
       copy s

  store = do
    inc <- await
    case inc of
         Nothing -> return ()    -- no more incoming data
         Just bs -> liftIO $ BL.hPut h_tmp bs -- store data

  copy s =
    -- copy local block of length s (TODO: use pipes!)
    liftIO $ BL.hGet h_loc s >>= BL.hPut h_tmp
