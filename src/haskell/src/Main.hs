module Main where

import Types
import Process
import Connection

main :: IO ()
main = runTrader (initState "CARBONFOURTEEN" "10.0.131.184" 0) (return ()) handshake
