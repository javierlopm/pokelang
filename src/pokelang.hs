module Main where
import System.Environment  
import Tokens
import Lexer

main = do
  argumentList:_ <- getArgs
  let fileContent = readFile(argumentList)
  s <- readFile argumentList
  let (hasErrors,tokens) = checkErrors $ lexer s
  if hasErrors then putStrLn "Lexing errors found: \n"
               else return ()

  ((mapM_ putStrLn) . (map show)) tokens
