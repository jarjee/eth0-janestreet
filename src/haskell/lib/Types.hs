{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE OverloadedStrings          #-}
module Types where

import Control.Applicative
import Control.Monad.State
import Data.Aeson
import Data.Text(Text)
import Network.Socket
import System.IO

newtype OrderId = OrderId Integer
    deriving (Show, Ord, Eq)

instance ToJSON OrderId where
    toJSON (OrderId x) = toJSON x 

newtype Symbol = Symbol Text
    deriving (Show, Ord, Eq)

instance ToJSON Symbol where
    toJSON (Symbol x) = toJSON x 

instance FromJSON Symbol where
    parseJSON (String x) = pure $ Symbol x

data SymbolWithQuantity = SymbolWithQuantity Symbol Quantity
    deriving (Show, Ord, Eq)

instance FromJSON SymbolWithQuantity where
    parseJSON (Object x) =  SymbolWithQuantity
                        <$> x .: "symbol"
                        <*> x .: "position"

newtype Price = Price Integer
    deriving (Show, Ord, Eq)

instance ToJSON Price where
    toJSON (Price x) = toJSON x 

newtype Quantity = Quantity Integer
    deriving (Show, Ord, Eq)

instance ToJSON Quantity where
    toJSON (Quantity x) = toJSON x

instance FromJSON Quantity where
    parseJSON x = Quantity <$> parseJSON x

newtype BookBuys  = BookBuys [(Price, Quantity)]
    deriving (Show, Ord, Eq)

newtype BookSells = BookSells [(Price, Quantity)]
    deriving (Show, Ord, Eq)

data Direction = Buy
               | Sell
    deriving (Show, Ord, Eq)

instance ToJSON Direction where
    toJSON Buy  = String "buy"
    toJSON Sell = String "sell"

data ClientMessage = ClientHello String
                   | Add OrderId Symbol Direction Price Quantity
                   | Convert OrderId Symbol Direction Quantity
                   | Cancel OrderId
    deriving (Show, Ord, Eq)

instance ToJSON ClientMessage where
    toJSON (ClientHello tn) =
        object [ "type" .= ("hello" :: Text)
               , "team" .= tn]
    toJSON (Add o s d p q) =
        object [ "type" .= ("add" :: Text) ]
  
data ServerMessage = ServerHello Integer [SymbolWithQuantity] Bool
                   | MarketOpen Bool
                   | Error String
                   | Book Symbol BookBuys BookSells
                   | Trade Symbol Price Quantity
                   | Ack OrderId
                   | Reject OrderId String
                   | Fill OrderId Symbol Direction Price Quantity
                   | Out OrderId
    deriving (Show, Ord, Eq)

instance FromJSON ServerMessage where
    parseJSON (Object x) = do
        ty <- x .: "type"
        case (ty :: Text) of
            "hello" -> ServerHello
                       <$> x .: "cash"
                       <*> x .: "symbols"
                       <*> x .: "market_open"
    parseJSON _ = error "Only expect objects" 
data TraderState = TraderState
    { teamName     :: String
    , traderHandle :: Handle
    } deriving (Show)

newtype Trader a = Trader {
    unTrader :: StateT TraderState IO a
} deriving (Functor, Applicative, Monad, MonadIO, MonadState TraderState)
