{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-name-shadowing #-}

module Sync2 where

import Control.Monad.Trans
import System.IO
--import System.Directory
import Text.ProtocolBuffers

import qualified Data.Map as M
import qualified Data.Set as S

import Sync2.IO
import Sync2.Hashing
import Sync2.Protocol

import Data.Conduit
--import Data.Conduit.List (sinkNull)

port :: Int
port = 8872

blocksize :: Int
blocksize = 5000

clF, srvF :: FilePath
clF  = "src.mkv"
srvF = "dest.mkv"

--main :: IO ()
--main = server

--upF :: FilePath
--upF  = "test-upload.txt"

condShow :: (MonadIO m, Show a) => Conduit a m a
condShow = awaitForever $ \a -> do
  liftIO (print a)
  yield a

server :: IO ()
server = runServer (serverSettings port HostAny) $ do

  -- get fileinfo
  --Just fi' <- getMsg
  fi <- getFileInfo srvF blocksize
  let fp    = toString     $ ft_filename fi
      s_blk = fromIntegral $ ft_blocksize fi

  -- get hashes & send out matching
  m_lookup <- srvGetRollingHash fi

  -- get matched/unmatched lookup map
  m_fl <- srvSendMatching fp m_lookup s_blk
  _lkup@(SrvLookupMatched m_final) <- srvGetMatched m_fl

  liftIO $ do
    putStrLn "\nMatched blocks:"
    mapM_ print $ M.toList m_final
    putStrLn ""

  --liftIO $ putStr "Receiving... " >> hFlush stdout
  --srvSaveUploadAs fp lkup upF
  --liftIO $ putStrLn "Done!"

client :: IO ()
client = runClient (clientSettings port "localhost") $ do

  let fp = clF

  -- send file info
  --send1 $ getFileInfoP fp s_blck $= fromMsg

  -- send rolling hashes
  clSendRollingHashes fp blocksize

  -- open local file
  h <- withBinaryFile' fp ReadMode

  -- get matching blocks (sends out unmatched blocks)
  _lkup@(ClLookupMatched blcks) <- clGetMatching h
  liftIO $ do
    putStrLn "\nMatched blocks:"
    mapM_ print $ S.elems blcks
    putStrLn ""

  --liftIO $ putStr "Uploading... " >> hFlush stdout
  --clUpload h lkup
  --liftIO $ putStrLn "Done!"
