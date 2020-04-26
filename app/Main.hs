module Main where

import qualified Data.Text.IO as T
import System.IO
import Data.Text (Text)

main :: IO ()
main = go

go :: IO ()
go = do
    putStr "stql > "
    hFlush stdout
    input <- T.getLine
    hFlush stdout
    let result = process input
    printResult result
    case result of 
      Exit -> return ()
      _ -> go


data ReplResult = Exit | Continue | NoInput

process :: Text -> ReplResult
process input = case input of
                  "" -> NoInput
                  ".exit" -> Exit
                  _ -> Continue

printResult :: ReplResult -> IO ()
printResult NoInput = return ()
printResult Exit = return ()
printResult Continue = T.putStrLn "Error reading input."
