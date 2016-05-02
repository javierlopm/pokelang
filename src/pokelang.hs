module Main where
import System.Environment
import System.IO(hPutStrLn,stderr)  
import Tokens
import Grammar
import Lexer


main = do
  argumentList:_ <- getArgs
  let fileContent = readFile argumentList
  s <- readFile argumentList

  let (goods,errors,errorcount) = checkErrors' $ lexer s
  if null errors 
      then do mapM_ print goods
              mapM_ print $ (parser goods :: [Token])
              print "done"
      else do mapM_ print errors
              print $ "--pkcc: "++ show errorcount ++ " errors found."