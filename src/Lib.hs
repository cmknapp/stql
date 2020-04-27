module Lib (
    PromptInput (..),
    MetaCommand (..),
    StqlResult (..),
    parsePrompt,
    processInput,
    printResult
) where

import Data.Text.IO as TIO
import Data.Text as T

data PromptInput = PreparedStatement |
                   Meta MetaCommand |
                   NoInput

data MetaCommand = Exit | MetaUnrecognized Text

data StqlResult = StatementResult | CommandResult MetaCommand | NoResult

parsePrompt :: Text -> PromptInput
parsePrompt raw = if T.null input
                     then NoInput
                     else case firstChar of
                          '.' -> Meta (parseMetaCommand rest)
                          _ -> PreparedStatement
    where input = T.strip raw
          firstChar = T.head input
          rest = T.tail input

parseMetaCommand :: Text -> MetaCommand
parseMetaCommand "exit" = Exit
parseMetaCommand s = MetaUnrecognized s

processInput :: PromptInput -> StqlResult
processInput PreparedStatement  = StatementResult
processInput (Meta cmd) = CommandResult cmd
processInput NoInput = NoResult

printResult :: StqlResult -> IO ()
printResult NoResult = return ()
printResult (CommandResult Exit) = return ()
printResult (CommandResult (MetaUnrecognized keyword)) = TIO.putStrLn $ "Unrecognized command: " <> keyword
printResult StatementResult = TIO.putStrLn "Error reading input."
