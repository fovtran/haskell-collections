-- -- $ ghc -O --make -threaded Spell.hs -o spell
--  $ time ./spell test.txt 1 +RTS -N1
--  $ time ./spell 4 +RTS -N4

import Data.Set hiding (map)
import Data.Maybe
import Data.Char
import Text.Printf
import System.IO
import System.Environment
import Control.Concurrent
import Control.Monad

-- The entry point, modified to break the word list into chunks, and then dispatching a chunk to each thread:

    main = do
        (f, g, n) <- readFiles
        let dict = fromList (lines f)
            work = chunk n (words g)
        run n dict work

    chunk :: Int -> [a] -> [[a]]
    chunk _ [] = []
    chunk n xs = (take n xs) : (chunk n (drop n xs))

-- The 'run' function sets up a channel between the main thread and all children thread ('errs'), and prints spelling -- errors as they arrive on the channel from the children. It then forks off 'n' children threads on each piece of the 

    run n dict work = do
        chan <- newChan
        errs <- getChanContents chan    -- errors returned back to main thread
        mapM_ (forkIO . thread chan dict) (zip [1..n] work)
        wait n errs 0
		
    wait n xs i = when (i < n) $ case xs of
        Nothing : ys -> wait n ys $! i+1
        Just s  : ys -> putStrLn s >> wait n ys i
Each thread spell checks its own piece of the work list. If it finds a spelling error, it passes the offending word back over the channel to the main thread.

    thread chan dict (me, xs) = do
        mapM_ spellit xs
        writeChan chan Nothing

     where
        spellit w = when (spell dict w) $
            writeChan chan . Just $ printf "Thread %d: %-25s" (me::Int) w
-- The 'spell' function is simplified a little:

    spell d w = w `notMember` d
-- which we could also write as:

    spell = flip notMember
	
-- We modify the readFiles phase to take an additional numeric command line argument, specifying the number of threads to run:

    readFiles = do
        [s,n] <- getArgs
        f     <- readFile "/usr/share/dict/words"
        g     <- readFile s
        return (f,g, read n)

