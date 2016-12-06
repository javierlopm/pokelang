module Main where
import System.Environment
import System.Exit(exitFailure)
import System.IO(hPutStrLn,stderr)  
import qualified Data.Text.IO as T
import qualified Data.Text as To
import Tokens
import Grammar
import ErrorHandle
import Lexer
import TableTree
import Types
import GrammarMonad
import Instructions
import Data.Foldable(toList)
import InsToTac
import Instructions
import Tac(showP)
import TacToMips
import qualified Data.Sequence as S


myF :: String -> String -> (String,String)
myF arg1 arg2 = if and [arg1 /= "-l",arg1/="-p",arg1/="-i",arg1/="-a",arg1/="-tac",arg1/="-c"] 
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

dr1 :: (a,b,c) -> (a,b)
dr1 (a,b,c) = (a,b)

getIns :: [Token] -> Bool -> IO ([(String,Ins)])
getIns tokens pr = do
  let (ast,state,strlog) = run (parser tokens) "" initialState
  let (logs,errors,errorcount) = checkParseError strlog
  if errorcount == 0
    then do if pr then putStrLn $ printAsts (map dr1 ast) else return ()
            return $ map dr1 ast
    else printErrors errorcount id errors >> exitFailure >> return []
  
getIns' :: [Token] -> Bool -> IO (([(String,Ins,TypeTuple)],[Declare]))
getIns' tokens pr = do
  let (ast,state,strlog) = run (parser tokens) "" initialState
  let (logs,errors,errorcount) = checkParseError strlog

  if errorcount == 0
    then do if pr then putStrLn $ printAsts (map dr1 ast) else return ()
            return (ast,strTbl state)
    else printErrors errorcount id errors >> exitFailure >> return ([],[])

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
          "-i"      -> getIns goods True >> return ()
          "-tac"    -> do (ast,strs) <- getIns' goods False
                          programs <- evalTree (forestToTac' ast) initTranslator
                          let full_prog = (("",translateStrings strs):programs)
                          putStrLn $ foldl (\ b (string,p) -> b ++ "\n" ++ "\n" ++ showP p ) "" full_prog
                          return ()
          "-c"    -> do (ast,strs) <- getIns' goods False
                        programs <- evalTree (forestToTac' ast) initTranslator
                        let prog_blocks = (map partition) (map snd programs)
                        -- putStrLn $ show prog_blocks
                        -- putStrLn "FIN======================================"
                        program <- runCompiler (mapM compile prog_blocks) initDescriptor
                        crt     <- readFile "crt.asm"
                        T.putStrLn $ stringsToMips $ translateStrings strs
                        putStrLn ".text\nmain:\n"
                        (T.putStrLn . assembly . snd) program
                        -- let crt = ""
                        putStrLn crt
                        return ()
          otherwise -> print $ "Unrecognized argument" ++ runargs
      else do mapM_ print errors
              putStrLn $ "--pkcc: "++ show errorcount ++ " errors found."