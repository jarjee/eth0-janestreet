{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE OverloadedStrings          #-}
module Types where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Data.Aeson
import Data.Text(Text)

import Network.Socket

newtype OrderId = OrderId Integer
    deriving (Show, Ord, Eq)

newtype Symbol = Symbol String
    deriving (Show, Ord, Eq)

newtype Price = Price Integer
    deriving (Show, Ord, Eq)

newtype Quantity = Quantity Integer
    deriving (Show, Ord, Eq)

newtype BookBuys  = BookBuys [(Price, Quantity)]
    deriving (Show, Ord, Eq)

newtype BookSells = BookSells [(Price, Quantity)]
    deriving (Show, Ord, Eq)

data Direction = Buy
               | Sell
    deriving (Show, Ord, Eq)

data ClientMessage = ClientHello String
                   | Add OrderId Symbol Direction Price Quantity
                   | Convert OrderId Symbol Direction Quantity
                   | Cancel OrderId
    deriving (Show, Ord, Eq)

instance ToJSON ClientMessage where
    toJSON (ClientHello tn) = object [ "type" .= ("hello" :: Text)
                                     , "team" .= tn]

data ServerMessage = ServerHello Integer [Symbol] Bool
                   | MarketOpen Bool
                   | Error String
                   | Book Symbol BookBuys BookSells
                   | Trade Symbol Price Quantity
                   | Ack OrderId
                   | Reject OrderId String
                   | Fill OrderId Symbol Direction Price Quantity
                   | Out OrderId
    deriving (Show, Ord, Eq)

data TraderState = TraderState
    { teamName       :: String
    , traderSocket   :: Socket
    , traderSockAddr :: SockAddr
    } deriving (Show)

newtype Trader a = Trader {
    unTrader :: StateT TraderState IO a
} deriving (Functor, Applicative, Monad, MonadIO, MonadState TraderState)
