module Process where

import Control.Monad
import Control.Monad.State

import Types

runTrader :: IO TraderState
          -> Trader ()
          -> Trader a
          -> IO a
runTrader initState cleanup collect =
    let collect' = unTrader $ do
        result <- collect
        cleanup
        return result
    in initState >>= evalStateT collect'
