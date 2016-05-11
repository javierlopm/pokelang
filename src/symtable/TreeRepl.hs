import TableTree


repl zipper = do
    putStr ">"
    command <- getLine
    case command of 
        "help"  -> do putStrLn "Use one of the following commands:"
                      putStrLn "    show  insert  enter  quit"
                      putStrLn "    up  down left right  top"
        "down"  -> do case down zipper of 
                        Nothing   -> putStrLn "No child to visit"
                        Just zipp -> repl zipp
        "show"  -> print zipper
        "enter" -> repl $ apply enterScope zipper 
        "up"    -> putStrLn "chao"
        "left"  -> putStrLn "chao"
        "right" -> putStrLn "chao"
        "top"   -> putStrLn "chao"
        "quit"  -> putStrLn "chao"
        blah    -> case head (words blah) of 
                    "insert" -> do let key = head $ tail $        words blah
                                   let val = head $ tail $ tail $ words blah
                                   let intVal = read val :: Int
                                   repl $ apply (insert key intVal) zipper
                    _        -> putStrLn "Use 'help'"
                                
    repl zipper

                                



main = repl $ fromScope (emptyScope::Scope Int)
