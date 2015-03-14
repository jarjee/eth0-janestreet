{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings          #-}

module Connection where

import Control.Applicative
import Control.Monad.State
import Data.Aeson hiding (Error)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy  as BSL
import Data.Maybe
import Data.Monoid
import Network.BSD
import Network.Socket hiding (recv, send, sendTo)
import Network.Socket.ByteString
import System.IO

import Types

initState :: String -> String -> Int -> IO TraderState
initState teamName addr ix = do
    sock <- socket AF_INET Stream defaultProtocol
    hAddr <- inet_addr addr
    let sockAddr = SockAddrInet (fromIntegral $ 25000 + ix) hAddr
    connect sock sockAddr
    h <- socketToHandle sock ReadWriteMode
    return $ TraderState teamName h


handshake :: Trader ()
handshake = do
    s@TraderState{..} <- get
    liftIO $ print s
    sendMessage $ ClientHello teamName
    liftIO $ putStrLn "Sent hello"
    msg <- recvMessage
    liftIO $ print msg
    liftIO $ putStrLn "Handshake complete"

sendMessage :: ClientMessage -> Trader ()
sendMessage message = do
    TraderState{..} <- get
    let msg = BSL.toStrict $ encode message
    liftIO $ BSC.hPutStrLn traderHandle msg

recvMessage :: Trader ServerMessage
recvMessage = do
    TraderState{..} <- get
    liftIO $ putStrLn "getting response"
    msg <- liftIO $ BSL.fromStrict <$> BSC.hGetLine traderHandle
    liftIO $ print msg
    return $ fromJust $ decode msg
