module Main where
import System.Environment
import System.IO(hPutStrLn,stderr)  
import Tokens
import Lexer


main = do
  argumentList:_ <- getArgs
  let fileContent = readFile(argumentList)
  s <- readFile argumentList
  let tokens = lexer s

  putStrLn ""

  ((mapM_ checkErrors) ) tokens
