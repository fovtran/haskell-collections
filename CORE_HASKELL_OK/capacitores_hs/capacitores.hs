import System.Environment

main = do
    args <- getArgs
    let arg = args !! 3

    print args
    putStrLn arg

    progName <- getProgName
    putStrLn progName