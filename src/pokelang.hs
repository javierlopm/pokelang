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
import Instructions
import Data.Foldable(toList)


myF :: String -> String -> (String,String)
myF arg1 arg2 = if and [arg1 /= "-l",arg1/="-p",arg1/="-i",arg1/="-a"] 
                    then (arg1,arg2)
                    else (arg2,arg1)

-- Print parser output and lexers from tokens if printLex it's true
execParser :: Bool -> [Token] -> IO()
execParser printLex tokens = do
  if printLex then mapM_ print tokens >> putStrLn ""
              else return ()
  let (state,strlog) = exec (parser tokens) "" initialState
  -- putStrLn "Log: \n======================="
  -- mapM_ print $ toList $ strlog
  let (logs,errors,errorcount) = checkParseError strlog
  if errorcount == 0
    then do let (strs,enu,scps) =  makeTable state -- $ fromZipper state
            putStrLn $ "Enums: \n======================="  ++ show enu
            putStrLn $ "Strings: \n======================="  ++ show strs
            putStrLn $ "SymTable:\n========================" ++ show scps
    else do printErrors errorcount id errors

getIns :: [Token] -> IO()
getIns tokens = do
  let (ast,state,strlog) = run (parser tokens) "" initialState
  let (logs,errors,errorcount) = checkParseError strlog
  if errorcount == 0
    then print ast
    else printErrors errorcount id errors
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
                "-i"      -> getIns  goods
                otherwise -> print $ "Unrecognized argument" ++ runargs
      else do mapM_ print errors
              putStrLn $ "--pkcc: "++ show errorcount ++ " errors found."