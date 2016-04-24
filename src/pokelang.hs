module Main where
import System.Environment  
import Tokens
import Lexer

main = do
  argumentList:_ <- getArgs
  let fileContent = readFile(argumentList)
  s <- readFile argumentList
  let tklist   = lexer s
  print tklist