{- The authors of this work have released all rights to it and placed it
in the public domain under the Creative Commons CC0 1.0 waiver
(http://creativecommons.org/publicdomain/zero/1.0/).

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Retrieved from: http://en.literateprograms.org/Goldbach's_Conjecture_(Haskell)?oldid=18656
-}

module Main (goldbach, main) where

import Prelude
import System.Environment
import System.IO (hFlush, stdout)
import Primes (primes)             -- Updated 10/19/09, Added module (see below)

{- The Goldbach Conjecture: An Implementation
   James Brannan (irregularexpression@gmail.com)
-}
    

-- For convenience
type Pair = (Integer, Integer)

{- showPList: Prints a list of Pairs and formats them as a
              set into columns between braces. -}
showPList :: [Pair] -> IO ()
showPList [] = putStr "{ }\n"
showPList plist = do
    putStr "{ " 
    fmt 1 plist
    putStr " }\n"  where
        showPair (x, y) = "(" ++ show x ++ "+" ++ show y ++ ")"
        fmt n (y:ys) = do
            putStr (showPair y)
            -- Every 4 elements, start a new column.
            if n `mod` 4 == 0 then 
                if ys == [] then return () else putStr "\n  "
             else putStr "   "
            fmt (n+1) ys
        fmt _ [] = return ()
                
                
{- findsum: Finds a sum x by zipping a list (of primes) less than
            x with itself (excluding repeats) and checking for 
            pairs that add up to x. I remove a lot of the garbage
            by throwing away values
            of z greater than y (to remove [x,y] == [y,x]). -}
findsum :: Integer -> [Integer] -> [Pair]
findsum x plist = [(y,z) | y <- plist, z <- plist, y+z == x, y<=z]


{- goldbach: Takes a positive even integer x and exhaustively checks
             pairs of primes less than that integer that add up to
             precisely x. Note we chop off half the list -- each 
             pair has a non-unique equivalent => (a,b) = (b,a) -}
goldbach :: Integer -> [Pair]
goldbach x | odd x || x < 3 = error "Must be a positive even integer greater than 2"
           | otherwise = findsum x plist where
                -- Find list of primes smaller than x.
                plist = takeWhile (< x) primes


{- main: Driver interface for the defined functions. On first run,
         an introduction message will be displayed, otherwise only
         the prompt will be shown. -}
main :: IO ()
main = do
    args <- getArgs
    let args_num = [(read x::Integer) | x <- args]
    case args of
        [] -> helper True                 -- No arguments, run interactive
        _  -> mapM_ showvalue args_num    -- Arguments given, run through
    
    where
    -- I *hate* nested paren's (too much lisp...) Use $!
    showvalue x = do putStr $ resultMsg x $ length l
                     showPList l
        where l = goldbach x
        
    helper b = do
        intro b    -- Show intro message depending on b
        {- I have to import some IO functions here
           because by default the output is buffered
           and won't print until an \n is found, meaning
           user input can't be on the same line as the
           prompt. Without flushing the output first, 
           the program executes out of order (when compiled).
        -}
        putStr "[Number or 0] -> "
        hFlush stdout
        x <- readLn :: IO Integer -- Get input and calculate or exit
        exec x
    
    -- Intro message to be shown when the program is first run
    intro True = putStr ("The Goldbach Conjecture states that any positive, " ++
                         "even integer (>2) can be expressed as the sum of " ++
                         "two prime numbers. This application is an " ++
                         "algorithmic solver for the Goldbach Conjecture. " ++
                         "Begin by entering values, and the appropriate sets " ++
                         "of prime numbers will be returned. You may quit at " ++
                         "any time by typing the number 0.\n\n")
    intro False = return ()

    resultMsg int res = "\nGoldbach(" ++ show int ++ ") = " ++ show res ++ " unique sets:\n"
        
    -- Allow user to quit by typing 0
    exec 0 = putStr "\tQuitting...\n"
    exec y = do showvalue y
             putStr "\n"
             helper False -- Call without intro msg until user quits.


-- James Brannan (irregularexpression@gmail.com) 9/18/09



