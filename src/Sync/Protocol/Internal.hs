{-# LANGUAGE ConstraintKinds #-}

module Sync.Protocol.Internal where

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State
import Data.Conduit
--import Data.Conduit.Network
import Data.Conduit.Network.Stream
import Data.ByteString (ByteString)
import qualified Data.Conduit.List            as CL

type NetApp    m = StateT  (NetState  m) (ReaderT         (NetData m)   (ResourceT m))
type NetStream m = ResourceT m
type NetData   m = StreamData (NetStream m)
type NetState  m = Either  (NetData   m) (ResumableSource (NetStream m) (ByteString))

type MonadResourceBase m = (MonadIO m, MonadUnsafeIO m, MonadThrow m, Applicative m)
type NetSendable a m = Sendable (ResourceT m) a

-- for debugging only really
condShow :: (MonadIO m, Show a) => String -> Conduit a m a
condShow prefix = CL.mapM $ \bs -> do
  liftIO $ putStrLn $ prefix ++ show bs
  return bs
