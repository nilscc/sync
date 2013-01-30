module Sync2.Protocol.Internal where

import Control.Monad.Trans
import Control.Monad.Reader
import Data.Conduit
import Data.Conduit.Network
import Data.ByteString (ByteString)
import qualified Data.Conduit.List     as CL
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as B8
import Data.String

import Sync2.Protocol.VarInt

type NetApp m = ResourceT (ReaderT (AppData m) m)

-- | Fixed length ByteString packet
newtype SizedBS = SizedBS { sized_raw :: ByteString }
  deriving (Eq, Show)

instance IsString SizedBS where
  fromString s = let bs = B8.pack s
                  in SizedBS $ BS.pack (varint (BS.length bs)) `BS.append` bs

withRaw :: Monad m => Pipe ByteString ByteString o u m r -> Pipe SizedBS SizedBS o u m r
withRaw = mapInput (sized_raw) (Just . SizedBS) 

sendSizedBS :: MonadIO m => Sink SizedBS (NetApp m) ()
sendSizedBS = do
  ad <- lift ask
  liftIO $ putStrLn "snd: <raw>"
  withRaw $ transPipe (lift.lift) (appSink ad)

-- for debugging only really
condShow :: (MonadIO m, Show a) => String -> Conduit a m a
condShow prefix = CL.mapM $ \bs -> do
  liftIO $ putStrLn $ prefix ++ show bs
  return bs
