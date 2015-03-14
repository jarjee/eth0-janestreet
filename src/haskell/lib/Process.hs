{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}

module Process where

import Control.Applicative
import Control.Monad.State
import Data.Map(Map)
import qualified Data.Map as M
import Data.Monoid
import Data.Sequence((|>), ViewL(..))
import qualified Data.Sequence as Sq
import Network.BSD
import Network.Socket
import System.IO

import Connection
import Types

runTrader :: IO TraderState
          -> Trader ()
          -> Trader a
          -> IO a
runTrader initialise cleanup collect =
    let collect' = unTrader $ do
            result <- collect
            cleanup
            return result
    in initialise >>= evalStateT collect'

initState :: String -> String -> Int -> IO TraderState
initState teamName addr ix = do
    sock <- socket AF_INET Stream defaultProtocol
    hAddr <- inet_addr addr
    let sockAddr = SockAddrInet (fromIntegral $ 25000 + ix) hAddr
    connect sock sockAddr
    h <- socketToHandle sock ReadWriteMode
    return $ TraderState teamName h 15 mempty mempty mempty 0 0

processMessage :: ServerMessage -> Trader ()
processMessage  ServerHello{}       = return ()
processMessage (MarketOpen False)   = return ()
processMessage (Error str)          = liftIO $ putStrLn $ "we got an error: " <> str
processMessage (Book sym bb bs)     = do
    s@TraderState{..} <- get
    put s{marketState=M.insert sym (bb, bs) marketState}
processMessage (Trade sym (Price p) (Quantity q)) = do
    updateBuffer sym p q
processMessage (Ack _)              = return ()
processMessage (Reject oid str)     = liftIO $ putStrLn $ "we got a rejection for order: " <> show oid <> ", rejection reason: " <> str
processMessage (Fill oid sym d p q) = return () --TODO
processMessage (Out _)              = return ()

updateBuffer :: Symbol -> Integer -> Integer -> Trader ()
updateBuffer sym p q = do
    s@TraderState{..} <- get
    let newEntries = case M.lookup sym stockEntries of
            Nothing -> M.insert sym (Sq.singleton(p,q), q, p*q) stockEntries
            Just (buffer, q', weightedTotal) ->
                if | (windowSize == Sq.length buffer) ->
                       let (oldP, oldQ):<xs = Sq.viewl buffer
                           newBuffer        = xs |> (p, q)
                           newTotal         = weightedTotal - (oldP * oldQ) + (p * q)
                           newQuantity      = q' - oldQ + q
                       in M.insert sym (newBuffer, newQuantity, newTotal) stockEntries
                   | otherwise   ->
                       let newBuffer    = buffer |> (p, q)
                           newTotal     = weightedTotal  + (p * q)
                           newQuantity  = q' + q
                       in M.insert sym (newBuffer, newQuantity, newTotal) stockEntries
    put s{stockEntries=newEntries}

viewAverages :: Trader (Map Symbol Double)
viewAverages = M.map (\(_, q, t) -> (fromInteger t / fromInteger q)) <$> stockEntries <$> get

rawProcessAll :: Trader ()
rawProcessAll = forever $ do
    recvMessage >>= processMessage
    s@TraderState{..} <- get
    viewAverages >>= (liftIO . print)
    put s{numMessages=numMessages+1}
