module Sync.Local.Filetransfer where

{- OLD CODE:

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
