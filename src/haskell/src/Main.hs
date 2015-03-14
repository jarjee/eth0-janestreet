module Main where

import System.Environment

import Types
import Process
import Connection

main :: IO ()
main = do
    [addr, ix'] <- getArgs
    let ix = read ix'
    runTrader (initState "OHAITHAR" addr ix) (return ()) handshake
