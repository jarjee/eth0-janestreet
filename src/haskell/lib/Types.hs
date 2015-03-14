{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE OverloadedStrings          #-}
module Types where

import Control.Applicative
import Control.Monad.State
import Data.Aeson hiding (Error)
import Data.Text(Text)
import Data.Map(Map)
import qualified Data.Sequence as Sq
import Data.Sequence(Seq)
import System.IO
import Data.Vector((!))

data StockEntry = StockEntry (Seq Integer) Double
    deriving (Show, Ord, Eq)

newtype OrderId = OrderId Integer
    deriving (Show, Ord, Eq)

instance ToJSON OrderId where
    toJSON (OrderId x) = toJSON x

instance FromJSON OrderId where
    parseJSON x = OrderId <$> parseJSON x

newtype Symbol = Symbol Text
    deriving (Show, Ord, Eq)

instance ToJSON Symbol where
    toJSON (Symbol x) = toJSON x

instance FromJSON Symbol where
    parseJSON x = Symbol <$> parseJSON x

data SymbolWithQuantity = SymbolWithQuantity Symbol Quantity
    deriving (Show, Ord, Eq)

instance FromJSON SymbolWithQuantity where
    parseJSON (Object x) =  SymbolWithQuantity
                        <$> x .: "symbol"
                        <*> x .: "position"
    parseJSON _ = mzero

newtype Price = Price Integer
    deriving (Show, Ord, Eq)

instance ToJSON Price where
    toJSON (Price x) = toJSON x

instance FromJSON Price where
    parseJSON x = Price <$> parseJSON x

newtype Quantity = Quantity Integer
    deriving (Show, Ord, Eq)

instance ToJSON Quantity where
    toJSON (Quantity x) = toJSON x

instance FromJSON Quantity where
    parseJSON x = Quantity <$> parseJSON x

data BookEntry = BookEntry Price Quantity
    deriving (Show, Ord, Eq)

instance FromJSON BookEntry where
    parseJSON (Array x) = BookEntry
                       <$> (parseJSON $ x ! 0)
                       <*> (parseJSON $ x ! 1)
    parseJSON _ = mzero

newtype BookBuys = BookBuys [BookEntry]
    deriving (Show, Ord, Eq)

instance FromJSON BookBuys where
    parseJSON x = BookBuys <$> parseJSON x

newtype BookSells = BookSells [BookEntry]
    deriving (Show, Ord, Eq)

instance FromJSON BookSells where
    parseJSON x = BookSells <$> parseJSON x

data Direction = Buy
               | Sell
    deriving (Show, Ord, Eq)

instance ToJSON Direction where
    toJSON Buy  = String "buy"
    toJSON Sell = String "sell"

instance FromJSON Direction where
    parseJSON (String "buy")  = pure Buy
    parseJSON (String "sell") = pure Sell
    parseJSON _ = mzero
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
        object [ "type"     .= ("add" :: Text)
               , "order_id" .= o
               , "symbol"   .= s
               , "dir"      .= d
               , "price"    .= p
               , "size"     .= q
               ]
    toJSON (Convert o s d q) =
        object [ "type" .= ("convert" :: Text)
               , "order_id" .= o
               , "symbol"   .= s
               , "dir"      .= d
               , "size"     .= q
               ]
    toJSON (Cancel o) =
        object [ "type" .= ("cancel" :: Text)
               , "order_id" .= o
               ]
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
            "hello" -> ServerHello <$> x .: "cash" <*> x .: "symbols" <*> x .: "market_open"
            "market_open" -> MarketOpen <$> x .: "open"
            "error"  -> Error  <$> x .: "error"
            "book"   -> Book   <$> x .: "symbol" <*> x .: "buy" <*> x .: "sell"
            "trade"  -> Trade  <$> x .: "symbol" <*> x .: "price" <*> x .: "size"
            "ack"    -> Ack    <$> x .: "order_id"
            "reject" -> Reject <$> x .: "order_id" <*> x .: "error"
            "fill"   -> Fill   <$> x .: "order_id" <*> x .: "symbol" <*>
                                   x .: "dir" <*> x .: "price" <*> x .: "size"
            "out"    -> Out    <$> x .: "order_id"
            _        -> mzero
    parseJSON _ = error "Only expect objects"

data TraderState = TraderState
    { teamName     :: String
    , traderHandle :: Handle
    , windowSize   :: Int
    , stockEntries :: Map Symbol (Seq (Integer, Integer), Integer, Integer) -- Moving average of trades
    , inventory    :: Map Symbol Int
    , marketState  :: Map Symbol (BookBuys, BookSells)
    , cash         :: Integer
    , numMessages  :: Int
    } deriving (Show)

newtype Trader a = Trader {
    unTrader :: StateT TraderState IO a
} deriving (Functor, Applicative, Monad, MonadIO, MonadState TraderState)
