module Main where

import System.Environment
import Parser

main :: IO ()
main = do
        args <- getArgs
        fileContents <- readFile $ (args !! 0)
        putStrLn (show $ parseAst fileContents)
