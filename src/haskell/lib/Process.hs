{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}

module Process where

import Control.Monad.State
import qualified Data.Map as M
import Data.Monoid
import Data.Sequence(ViewL(..))
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
    return $ TraderState teamName False h 7 2 mempty mempty mempty mempty mempty 0 1

processMessage :: ServerMessage -> Trader ()
processMessage (ServerHello c sq b) = do
    st@TraderState{..} <- get
    let inv = M.fromList $ map (\(SymbolWithQuantity s (Quantity q)) -> (s,q)) sq
       in put st{marketOpen=b, cash=c, inventory=inv}
processMessage (MarketOpen b)       = do
    s@TraderState{..} <- get
    put s{marketOpen=b}
processMessage (Error str)          = liftIO $ putStrLn $ "we got an error: " <> str
processMessage (Book sym bb bs)     = do
    s@TraderState{..} <- get
    put s{marketState=M.insert sym (bb, bs) marketState}
processMessage (Trade sym (Price p) (Quantity q)) = updateBuffer sym p q >> considerTrade
processMessage (Ack _)              = return ()
processMessage (Reject oid str)     = liftIO $ putStrLn $ 
    "we got a rejection for order: " <> show oid <> ", rejection reason: " <> str
processMessage (Fill oid sym d p q) = updateFill oid sym d p q
processMessage (Out _)              = return ()

updateFill :: OrderId -> Symbol -> Direction -> Price -> Quantity -> Trader ()
updateFill oid sym Buy p q = do
    s@TraderState{..} <- get
    case M.lookup sym ourBuyOrders of
        Nothing -> liftIO $ hPutStrLn stderr $
            "Warning: received a buy fill for oid: " <> show oid <>
            " but we had no entry for the symbol, " <> show sym
        Just iMap -> do
            case M.lookup oid iMap of
                Nothing -> liftIO $ hPutStrLn stderr $
                    "Warning: received a buy fill for oid: " <> show oid <>
                    " but we had no entry for the oid. symbol: " <> show sym
                Just (p', q') -> do
                    updateInventory sym (fromIntegral q')
                    updateCash $ (-1) * (fromIntegral p') * (fromIntegral q')
                    let newIMap = M.insert oid (p'-p, q'-q) iMap
                    put s{ourBuyOrders=M.insert sym newIMap ourBuyOrders}
updateFill oid sym Sell p q = do
    s@TraderState{..} <- get
    case M.lookup sym ourSellOrders of
        Nothing -> liftIO $ hPutStrLn stderr $
            "Warning: received a sell fill for oid: " <> show oid <>
            " but we had no entry for the symbol, " <> show sym
        Just iMap -> do
            case M.lookup oid iMap of
                Nothing -> liftIO $ hPutStrLn stderr $
                    "Warning: received a sell fill for oid: " <> show oid <>
                    " but we had no entry for the oid. symbol: " <> show sym
                Just (p', q') -> do
                    updateInventory sym (fromIntegral $ -q')
                    updateCash $ (fromIntegral p') * (fromIntegral q')
                    let newIMap = M.insert oid (p'-p, q'-q) iMap
                    put s{ourSellOrders=M.insert sym newIMap ourSellOrders}

updateCash :: Integer -> Trader ()
updateCash x = do
    s <- get
    put s{cash=cash s - x}

updateInventory :: Symbol -> Integer -> Trader ()
updateInventory sym x = do
    s <- get
    put s{inventory=M.insertWith (+) sym x (inventory s)}

updateBuffer :: Symbol -> Integer -> Integer -> Trader ()
updateBuffer sym p q = do
    s@TraderState{..} <- get
    let newEntries = case M.lookup sym stockEntries of
            Nothing -> M.insert sym (tradeToGaussian p q) stockEntries
            Just g@(Gaussian vals _ _ _) ->
                if | (windowSize == Sq.length vals) ->
                       M.insert sym (joinGaussian (removeLast g) (tradeToGaussian p q)) stockEntries
                   | otherwise   ->
                       M.insert sym (joinGaussian g (tradeToGaussian p q)) stockEntries
    put s{stockEntries=newEntries}

considerTrade :: Trader ()
considerTrade = do
    TraderState{..} <- get
    forM_ (M.toList stockEntries) $ (\(sym, g) -> do
        case M.lookup sym marketState of
            Just (BookBuys (bb:bbs), BookSells (bs:bss)) -> do
                let (bestBuy, buyQ)   = (\(BookEntry p q) -> (p, q)) bb
                let (bestSell, sellQ) = (\(BookEntry p q) -> (p, q)) bs
                let (bBuy, bSell) = bFactor g kFactor bestBuy bestSell
                when (bBuy  > 0.8) $ sell sym 100 bestBuy
                when (bSell < 0.2) $ buy  sym 100 bestSell
            Just _ -> liftIO $ putStrLn "no buy orders lulz"
            Nothing -> return ())

buy :: Symbol -> Quantity -> Price -> Trader ()
buy sym q p = do
    s@TraderState{..} <- get
    sendMessage $ Add (OrderId numOrders) sym Buy p q 
    let newNumOrders = numOrders+1
    let newOrders = M.insert sym (M.singleton (OrderId numOrders) (p, q)) ourBuyOrders 
    put s{ourBuyOrders = newOrders, numOrders = newNumOrders}

sell :: Symbol -> Quantity -> Price -> Trader ()
sell sym q p = do
    s@TraderState{..} <- get
    sendMessage $ Add (OrderId numOrders) sym Sell p q 
    let newNumOrders = numOrders+1
    let newOrders = M.insert sym (M.singleton (OrderId numOrders) (p, q)) ourSellOrders 
    put s{ourSellOrders = newOrders, numOrders = newNumOrders}

bFactor :: Gaussian -> Int -> Price ->  Price -> (Double, Double)
bFactor g k (Price bestBuy) (Price bestSell) =
    let [b1, b2] = map (\p -> (fromIntegral p - lower g k) / (upper g k - lower g k)) [bestBuy, bestSell]
    in (b1, b2)
upper :: Gaussian -> Int -> Double
upper g@(Gaussian _ _ m _) k = m + sd g * fromIntegral k

center :: Gaussian -> Int -> Double
center g@(Gaussian _ _ m _) k = m

lower :: Gaussian -> Int -> Double
lower g@(Gaussian _ _ m _ ) k = m - sd g * fromIntegral k

sd :: Gaussian -> Double
sd (Gaussian _ n _ v) = sqrt (v / fromIntegral n)

rawProcessAll :: Trader ()
rawProcessAll = forever $ do
    recvMessage >>= processMessage

tradeToGaussian :: Integer -> Integer -> Gaussian
tradeToGaussian p q = Gaussian (Sq.singleton (p,q)) q (fromIntegral p) 0

joinGaussian :: Gaussian -> Gaussian -> Gaussian
joinGaussian (Gaussian vals1 n1 m1 v1) (Gaussian vals2 n2 m2 v2) =
  let vals' = vals1 <> vals2
      n'    = n1 + n2
      m'    = (fromIntegral n1*m1 + fromIntegral n2*m2) / (fromIntegral n')
      v'    = v1 + v2 + (fromIntegral n1 * m1 * m1) + (fromIntegral n2 * m2 * m2) - (fromIntegral n' * m' * m')
  in Gaussian vals' n' m' v'

removeLast :: Gaussian -> Gaussian
removeLast g@(Gaussian vals _ _ _) = joinGaussian g{vals=xs} g'
  where (oldP, oldQ):<xs = Sq.viewl vals
        g' = Gaussian mempty (fromIntegral $ -oldQ) (fromIntegral oldP) 0
