module Main where
import System.Environment
import System.IO(hPutStrLn,stderr)  
import Tokens
import Grammar
import Lexer
import TableTree
import Types
import Control.Monad.RWS.Strict
import Data.Foldable(toList)



myF :: String -> String -> (String,String)
myF arg1 arg2 = if arg1 /= "-l" && arg1/="-p" && arg1/="-a"  
                    then (arg1,arg2)
                    else (arg2,arg1)
main = do
  arg1:arg2:_ <- getArgs
  let (fileToRead,runargs)=myF arg1 arg2

  s                    <- readFile fileToRead

  let (goods,errors,errorcount) = checkErrors' $ lexer s
  if null errors 
      then case runargs of 
                "-l"      -> mapM_ print goods
                "-p"      -> do let (state,strlog) = execRWS (parser goods) "" initialState
                                putStrLn "Log:"
                                mapM_ print $ toList $ strlog
                                putStrLn "Table:"
                                print $ makeTable state -- $ fromZipper state
                "-a"      -> do mapM_ print goods
                                putStrLn "\n" 
                                let (state,strlog) = execRWS (parser goods) "" initialState
                                putStrLn "Log:"
                                print strlog
                                putStrLn "Table:"
                                print $ makeTable state -- $ fromZipper state
                otherwise -> print $ "Unrecognized argument" ++ runargs

      else do mapM_ print errors
              print $ "--pkcc: "++ show errorcount ++ " errors found."
  where initialState = (ScopeNZip emptyScope (fromScope emptyScope) 0)
