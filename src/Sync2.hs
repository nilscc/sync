{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-name-shadowing #-}

module Sync2 where

import Control.Monad.Trans
import System.IO
import System.Directory
import Text.ProtocolBuffers

import qualified Data.Conduit.Binary as CB

import Sync2.IO
import Sync2.Hashing
import Sync2.Protocol

port :: Int
port = 8872

server :: IO ()
server = runServer (serverSettings port HostAny) $ do

  -- get fileinfo
  (next,Just fi) <- receive $$+ getMsg
  let fp    = toString     $ ft_filename fi
      s_blk = fromIntegral $ ft_blocksize fi

  h <- withBinaryFile' fp ReadMode

  -- get hashes & send out matching
  (next, m_lookup) <- next $$++ getRollingHash fi
  putMatching h m_lookup s_blk $$ sendMsg

  -- receive MD4 hashes and send out locations of matched blocks
  -- (receiving pipe can be closed)
  matches <- next $$+- condToMsg' =$ checkMatching h =$ andReturn sendMsg

  liftIO $ mapM_ print matches
  -- send out binary blocks of unmatched data
  --sendUnmatched h matches

client :: IO ()
client = runClient (clientSettings port "localhost") $ do

  let fp     = "Sync2.hs"
      s_blck = 300

  -- send file info
  getFileInfoP fp s_blck $$ sendMsg

  h_loc <- withBinaryFile' fp ReadMode

  -- send rolling hashes
  CB.sourceFile fp $$ putRollingHash s_blck =$ send

  -- send MD4 hashes on requested blocks
  (next,()) <- receive $$+ condToMsg' =$ hashMatching h_loc =$ sendMsg

  -- receive matching blocks
  (next,matches) <- next $$++ getMatching

  liftIO $ mapM_ print matches
  -- open temporary file and receive incoming data
  --(f_tmp,h_tmp) <- withBinaryTempFile' "." (fp ++ ".part") False
  --next $$+- receiveUnmatched h_loc h_tmp matches -- (close pipe!)

  -- rename files
  --liftIO $ renameFile f_tmp fp
