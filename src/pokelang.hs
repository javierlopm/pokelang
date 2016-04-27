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

  let myList= (map checkErrors) tokens
  let (ioList,errorList) = unzip myList
  sequence ioList
  let errorCount = sum errorList
  if errorCount > 0 then putStrLn $ "--pkcc: "++ show errorCount ++ " errors found.\n"
  else return()
