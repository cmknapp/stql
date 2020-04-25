module Lib
    ( someFunc
    ) where

import Data.Text.IO as T

someFunc :: IO ()
someFunc = T.putStrLn "someFunc"
