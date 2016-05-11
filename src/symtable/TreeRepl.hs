import System.Exit(exitSuccess)
import System.IO
import TableTree

move moveFuc zipper = do
  do case moveFunc zipper of 
      Nothing   -> putStrLn "No node to visit"
      Just zipp -> repl zipp

repl zipper = do
    putStr ">"
    command <- getLine
    case command of 
        "help"  -> do putStrLn "Use one of the following commands:"
                      putStrLn "    show  insert  enter  quit"
                      putStrLn "    up  down left right  top"
        "show"  -> print zipper
        "enter" -> repl $ apply enterScope zipper 
        "down"  -> move goDown zipper
        "up"    -> move goUp   zipper
        "left"  -> move goLeft zipper
        "right" -> move goRight zipper
        "top"   -> repl $ goTop zipper
        "quit"  -> putStrLn "chao" >> exitSuccess
        blah    -> case head (words blah) of 
                    "insert" -> do let key = head $ tail $        words blah
                                   let val = head $ tail $ tail $ words blah
                                   let intVal = read val :: Int
                                   repl $ apply (insert key intVal) zipper
                    _        -> putStrLn "Use 'help'"
                                
    repl zipper

                                



main = hSetBuffering stdout NoBuffering >> repl $ fromScope (emptyScope::Scope Int)
