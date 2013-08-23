{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-name-shadowing #-}

module Sync where

import Control.Monad.Trans

import Sync.Local
import Sync.Remote
import Sync.Protocol

port :: Int
port = 8872

blocksize :: Int
blocksize = 5000

clF :: FilePath
clF = "test.mkv"

server
  :: FilePath   -- ^ Base directory
  -> IO ()
server dir = runServer (serverSettings dir port HostAny) $ do

  compareFileRemote

client
  :: FilePath   -- ^ Base directory
  -> IO ()
client dir = runClient (clientSettings dir port "localhost") $ do

  matches <- compareLocalFile clF blocksize

  liftIO $ do
    putStrLn "Matched blocks:"
    mapM_ print matches
