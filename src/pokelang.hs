module Main where
import System.Environment
import System.IO(hPutStrLn,stderr)  
import Tokens
import Grammar
import Lexer


main = do
  fileToRead:runargs:_ <- getArgs
  s                    <- readFile fileToRead

  let (goods,errors,errorcount) = checkErrors' $ lexer s
  if null errors 
      then do case runargs of 
                "-l"      -> mapM_ print goods
                "-p"      -> mapM_ print (parser goods :: [Token])
                "-a"      -> do mapM_ print goods
                                putStrLn "\n"
                                mapM_ print (parser goods :: [Token])
                otherwise -> print $ "Unrecognized argument" ++ runargs

      else do mapM_ print errors
              print $ "--pkcc: "++ show errorcount ++ " errors found."