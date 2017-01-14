module Main where

import System.Environment
import Backend.Murphi.AST_Translation.TransMurphi
import Backend.Murphi.Print.MurphiPrint
import Frontend.Roc3.Parser


main :: IO ()
main = do
        args <- getArgs
        fileContents <- readFile $ (args !! 0)
        let name = args !! 1
        --putStrLn (show $ parseAst fileContents)
        let fAst      = parseAst fileContents
            bAst      = transform fAst
            murphiSrc = printMurphiProgram bAst
            targetName = if ('.' `elem` name) then name else name ++ ".m"
        writeFile targetName murphiSrc
