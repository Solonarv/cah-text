{-# LANGUAGE
    RecordWildCards,
    LambdaCase
    #-}

module CaHLike.Util.Network where

import Network.Connection

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import Data.Text (Text)
import qualified Data.Text as T

import System.IO.Unsafe (unsafePerformIO)

import Data.IORef

data ServerSettings = ServerSettings {
    serverAddress :: String,
    serverPort    :: Integer
    } deriving (Show, Eq)

defaultServerSettings = ServerSettings "localhost" 45678

toConnectionSettings :: ServerSettings -> ConnectionParams
toConnectionSettings ServerSettings{..} = ConnectionParams
    { connectionHostname  = serverAddress
    , connectionPort      = fromIntegral serverPort
    , connectionUseSecure = Just $ TLSSettingsSimple False False False
    , connectionUseSocks  = Nothing
    }

-- | Global state. This is fine, because we're single-threaded
globalContext :: IORef (Maybe ConnectionContext)
globalContext = unsafePerformIO $ newIORef Nothing
{-# NOINLINE globalContext #-}

getGlobalContext :: IO ConnectionContext
getGlobalContext = readIORef globalContext >>= \case
    Just ctx -> return ctx
    Nothing  -> error "global connection context not set"

initGlobalContext :: IO ()
initGlobalContext = initConnectionContext >>= writeIORef globalContext . Just

connectToServer :: ServerSettings -> IO Connection
connectToServer settings = getGlobalContext >>= flip connectTo (toConnectionSettings settings)