{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Types where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans

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

data TraderState = TraderState {
    teamName :: String
}
    deriving (Show, Ord, Eq)

newtype Trader m a = Trader {
    unTrader :: StateT TraderState m a
} deriving (Functor, Applicative, Monad)

deriving instance MonadIO m => MonadIO (Trader m)

instance MonadTrans Trader where
    lift = Trader . lift

