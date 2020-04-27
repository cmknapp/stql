module Main where

import qualified Data.Text.IO as TIO
import System.IO

import Lib

main :: IO ()
main = go

go :: IO ()
go = do
    putStr "stql > "
    hFlush stdout
    raw <- TIO.getLine
    hFlush stdout
    let result = processInput $ parsePrompt raw
    printResult result
    case result of 
      CommandResult Exit -> return ()
      _ -> go

