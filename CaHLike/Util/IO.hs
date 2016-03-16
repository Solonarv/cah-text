module CaHLike.Util.IO where

prompt :: String -> IO String
prompt s = putStrLn s >> getLine