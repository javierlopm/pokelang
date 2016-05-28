module Main where
import System.Environment
import System.IO(hPutStrLn,stderr)  
import Tokens
import Grammar
import ErrorHandle
import Lexer
import TableTree
import Types
import GrammarMonad
import Data.Foldable(toList)



myF :: String -> String -> (String,String)
myF arg1 arg2 = if arg1 /= "-l" && arg1/="-p" && arg1/="-a"  
                    then (arg1,arg2)
                    else (arg2,arg1)

-- Print parser output and lexers from tokens if printLex it's true
execParser :: Bool -> [Token] -> IO()
execParser printLex tokens = do
  if printLex then mapM_ print tokens >> putStrLn ""
              else return ()
  let (state,strlog) = exec (parser tokens) "" initialState
  -- putStrLn "Log:"
  -- mapM_ print $ toList $ strlog
  let (logs,errors,errorcount) = checkParseError strlog
  if errorcount == 0
    then do let (strs,scps) =  makeTable state -- $ fromZipper state
            putStrLn $ "Strings: \n======================="  ++ show strs
            putStrLn $ "SymTable:\n========================" ++ show scps
    else do printErrors errorcount id errors

main = do
  arg1:arg2:_ <- getArgs
  let (fileToRead,runargs)=myF arg1 arg2
  s  <- readFile fileToRead

  let (goods,errors,errorcount) = checkTokenError $ lexer s
  if null errors 
      then case runargs of 
                "-l"      -> mapM_ print goods
                "-p"      -> execParser False goods
                "-a"      -> execParser True  goods
                otherwise -> print $ "Unrecognized argument" ++ runargs
      else do print $ "--pkcc: "++ show errorcount ++ " errors found."