{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Proxy.TCP
import Network.Socket.ByteString

main = do
  putStrLn "Serving requests"
  serve HostAny "3000" $ \(connectionSocket, remoteAddr) -> do
    putStrLn $ "TCP connection established from " ++ show remoteAddr
    send connectionSocket "HTTP/1.1 204 No Content\r\n"
    return ()
