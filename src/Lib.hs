module Lib (
    PromptInput (..),
    MetaCommand (..),
    StqlResult (..),
    parsePrompt,
    processInput,
    printResult
) where

import           Data.Text    as T
import           Data.Text.IO as TIO

data PromptInput = PreparedStatement Statement |
                   Meta MetaCommand |
                   NoInput

data MetaCommand = Exit | MetaUnrecognized Text

data Statement = Insert Text | Select Text | Unrecognized --TODO: Replace Text

data StqlResult = StatementResult Statement | CommandResult MetaCommand | NoResult

parsePrompt :: Text -> PromptInput
parsePrompt raw = if T.null input
                     then NoInput
                     else case firstChar of
                          '.' -> Meta (parseMetaCommand rest)
                          _   -> PreparedStatement $ prepare input
    where input = T.strip raw
          firstChar = T.head input
          rest = T.tail input

parseMetaCommand :: Text -> MetaCommand
parseMetaCommand "exit" = Exit
parseMetaCommand s      = MetaUnrecognized s

processInput :: PromptInput -> StqlResult
processInput (PreparedStatement s) = execute s
processInput (Meta cmd)            = CommandResult cmd
processInput NoInput               = NoResult

prepare :: Text -> Statement
prepare input = case T.breakOn " " input of
                           ("insert", rest) -> Insert (T.tail rest)
                           ("select", rest) -> Select (T.tail rest)
                           _                -> Unrecognized

execute :: Statement -> StqlResult
execute s = StatementResult s

printResult :: StqlResult -> IO ()
printResult NoResult = return ()
printResult (CommandResult Exit) = return ()
printResult (CommandResult (MetaUnrecognized keyword)) = TIO.putStrLn $ "Unrecognized command: " <> keyword
printResult (StatementResult (Insert _)) = TIO.putStrLn "Prepared Statement"
printResult (StatementResult (Select _)) = TIO.putStrLn "Prepared Statement"
printResult (StatementResult Unrecognized) = TIO.putStrLn "Error reading input."
