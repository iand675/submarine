{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.ByteString (ByteString)
import Control.Proxy
import Control.Proxy.TCP
import Network.Socket.ByteString

main = do
  putStrLn "Serving requests"
  serve HostAny "3000" $ \(connectionSocket, remoteAddr) -> do
    {-putStrLn $ "TCP connection established from " ++ show remoteAddr-}
    runProxy (socketReadS 4096 connectionSocket >-> printD >-> mapD noContent >-> printD >-> socketWriteD connectionSocket) -- >-> noContentD >-> socketWriteD connectionSocket)
    {-send connectionSocket "HTTP/1.1 204 No Content\r\n\r\n"-}
    -- run code in pipe that is Request -> Response
    -- support timeouts
    return ()

{-noContentD :: (Proxy p, Monad m) => r -> Producer p ByteString m ()-}
noContent = const "HTTP/1.1 204 No Content\r\n\r\n"
