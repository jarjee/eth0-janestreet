{-# LANGUAGE RecordWildCards #-}

module Connection where

import Control.Applicative
import Control.Monad.State
import Data.Aeson hiding (Error)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy  as BSL
import Data.Monoid
import Network.BSD
import Network.Socket hiding (recv, send)
import Network.Socket.ByteString

import Types

initState :: String -> String -> Int -> IO TraderState
initState teamName addr ix = do
    sock <- socket AF_INET Stream defaultProtocol
    hAddr <- inet_addr addr
    let sockAddr = SockAddrInet (fromIntegral $ 25000 + ix) hAddr
    connect sock sockAddr
    return $ TraderState teamName sock sockAddr

handshake :: Trader ()
handshake = do
    s@TraderState{..} <- get
    liftIO $ print s
    sendMessage $ ClientHello teamName
    liftIO $ putStrLn "Sent hello"
    msg <- recvMessage
    liftIO $ print msg
    return ()

sendMessage :: ClientMessage -> Trader ()
sendMessage message = do
    TraderState{..} <- get
    let msg = BSL.toStrict $ encode message
    liftIO $ BSC.putStrLn msg
    numSent <- liftIO $ send traderSocket msg
    liftIO $ putStrLn $ "Sent " <> show numSent <> " bytes."

recvMessage :: Trader ServerMessage
recvMessage = do
    TraderState{..} <- get
    liftIO $ putStrLn "getting response"
    liftIO $ Error <$> BSC.unpack <$> recv traderSocket 100000
