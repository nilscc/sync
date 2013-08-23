module Sync.Protocol.Internal where

import           Control.Monad.Trans
import           Data.Conduit
import qualified Data.Conduit.List            as CL

-- for debugging only really
condShow :: (MonadIO m, Show a) => String -> Conduit a m a
condShow prefix = CL.mapM $ \bs -> do
  liftIO $ putStrLn $ prefix ++ show bs
  return bs
