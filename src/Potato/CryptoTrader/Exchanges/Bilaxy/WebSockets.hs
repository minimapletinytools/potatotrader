
module Potato.CryptoTrader.Exchanges.Bilaxy.WebSockets (
  mainSock
) where

import Wuss
import Control.Concurrent (forkIO)
import Control.Monad (forever, unless, void)
import Data.Text (Text, pack)
import Network.WebSockets (ClientApp, receiveData, sendClose, sendTextData)



--serv = "api.hubi.com"
--path = "/was"

serv = "bilaxy.com"
path = "/stream"

--serv = "echo.websocket.org"
--path = "/"


mainSock :: IO ()
mainSock = runSecureClient serv 443 path ws

ws :: ClientApp ()
ws connection = do
    putStrLn "Connected!"

    void . forkIO . forever $ do
        message <- receiveData connection
        print (message :: Text)

    let loop = do
            line <- getLine
            unless (null line) $ do
                sendTextData connection (pack line)
                loop
    loop

    sendClose connection (pack "Bye!")
