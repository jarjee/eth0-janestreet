{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings          #-}

module Connection where

import Control.Applicative
import Control.Monad.State
import Data.Aeson hiding (Error)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy  as BSL
import Data.Maybe

import Types

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
    liftIO $ fromJust <$> decode <$> BSL.fromStrict <$> BSC.hGetLine traderHandle
