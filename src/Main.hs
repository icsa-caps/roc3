module Main where

import System.Environment
import Parser
import TransMurphi
import Backend.Murphi.MurphiPrint


main :: IO ()
main = do
        args <- getArgs
        fileContents <- readFile $ (args !! 0)
        let name = args !! 1
        --putStrLn (show $ parseAst fileContents)
        let fAst      = parseAst fileContents
            bAst      = transform fAst
            murphiSrc = tomurphiTop bAst
            targetName = if ('.' `elem` name) then name else name ++ ".m"
        writeFile targetName murphiSrc
