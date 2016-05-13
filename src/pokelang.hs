module Main where
import System.Environment
import System.IO(hPutStrLn,stderr)  
import Tokens
import Grammar
import Lexer
import TableTree
import Control.Monad.RWS.Strict



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
                "-p"      -> do let (state,strlog) = execRWS (parser goods) "" (fromScope (emptyScope::Scope Pos))
                                putStrLn "Log:"
                                print strlog
                                putStrLn "Table:"
                                print $ fromZipper state
                "-a"      -> do mapM_ print goods
                                putStrLn "\n" 
                                let (state,strlog) = execRWS (parser goods) "" (fromScope (emptyScope::Scope Pos))
                                putStrLn "Log:"
                                print strlog
                                putStrLn "Table:"
                                print $ fromZipper state
                otherwise -> print $ "Unrecognized argument" ++ runargs

      else do mapM_ print errors
              print $ "--pkcc: "++ show errorcount ++ " errors found."