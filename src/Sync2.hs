{-# LANGUAGE OverloadedStrings #-}

module Sync2 where

import Control.Monad.Trans
import Data.Conduit
import Data.Conduit.Network
import Data.ByteString.Lazy.Char8 (unpack)

import qualified Data.Conduit.List   as CL
import qualified Data.Conduit.Binary as CB

import Sync2.Protocol
import Sync2.Hashing

p :: Int
p = 8872

server :: IO ()
server =
  runTCPServer (serverSettings p HostAny) $ \app -> do
    -- get filename
    (next,file) <- receive app $$+ getBS
    print (unpack file)
    -- get hashes
    hashes <- next $$+- condDecRolling =$ CL.consume
    mapM_ print hashes

client :: IO ()
client = runResourceT $
  runTCPClient (clientSettings p "localhost") $ \app -> do
    -- send filename
    putBS "Sync2.hs" $$ send app
    -- wait for user input
    _ <- liftIO $ getLine
    -- send hashes
    CB.sourceFile "Sync2.hs" $= condEncRolling 300 $$ send app
