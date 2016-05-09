module Main where
import System.Environment
import System.IO(hPutStrLn,stderr)  
import Tokens
import Grammar
import Lexer



myF :: String -> String -> (String,String)
myF arg1 arg2 = if arg1 /= "-l" && arg1/="-p" && arg1/="-a" then (arg1,arg2)
                else (arg2,arg1)
main = do
  arg1:arg2:_ <- getArgs
  let (fileToRead,runargs)=myF arg1 arg2

  s                    <- readFile fileToRead

  let (goods,errors,errorcount) = checkErrors' $ lexer s
  if null errors 
      then do case runargs of 
                "-l"      -> mapM_ print goods
                "-p"      -> do 
                              let myParse =  (parser goods :: [Token])
                              putStrLn $  (drop 2  (show myParse)) ++ "Succeed."
                "-a"      -> do mapM_ print goods
                                putStrLn "\n"
                                mapM_ print (parser goods :: [Token])
                otherwise -> print $ "Unrecognized argument" ++ runargs

      else do mapM_ print errors
              print $ "--pkcc: "++ show errorcount ++ " errors found."