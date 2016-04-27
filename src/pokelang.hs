module Main where
import System.Environment
import System.IO(hPutStrLn,stderr)  
import Tokens
import Lexer

-- Falta tratar los errores por salida estandar

main = do
  argumentList:_ <- getArgs
  let fileContent = readFile(argumentList)
  s <- readFile argumentList
  let tokens = lexer s
--  let (hasErrors,tokens) = (checkErrors . reverse . lexer) s
--  if hasErrors then putStrLn "Lexing errors found: \n"
--               else return ()
  putStrLn ""

  ((mapM_ checkErrors2) ) tokens
