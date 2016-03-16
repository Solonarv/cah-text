{-# LANGUAGE
    OverloadedStrings
    #-}

module CaHLike.Text.Client where

import CaHLike.Util.Network
import CaHLike.Util.IO

import Data.Text (Text)
import qualified Data.Text as T

import Network.Connection

main = do
    initGlobalContext
    addr <- prompt "Enter the server address:"
    port <- read <$> prompt "Enter the server port:"
    let srvSettings = ServerSettings addr port
    conn <- connectToServer srvSettings
    name <- prompt "Enter your name"