-- Haskell Unicode helpers

"让Haskell或者Ghci能正确显示汉字并且读取汉字命名的文档"
putStrLn "\35753Haskell\25110\32773Ghci\33021\27491\30830\26174\31034\27721\23383\24182\19988\35835\21462\27721\23383\21629\21517\30340\25991\26723"

print $ [(++"'s dad"), (++"'s mom")] <*> ["Simon", "John"]
["Simon's dad","John's dad","Simon's mom","John's mom"]

print $ [(++"の父"), (++"の母")] <*> ["田中", "山田"]
["\30000\20013\12398\29238","\23665\30000\12398\29238","\30000\20013\12398\27597","\23665\30000\12398\27597"]

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE XScopedTypeVariables #-}
-- :seti -XScopedTypeVariables
-- :set -XOverloadedStrings

Data.Text.IO.putStrLn "hello: привет"

newtype Vocable = Vocable2 { ortho :: String } deriving (Eq,Ord)
instance IsString Vocable -- to simplify typing the values (with OverloadedStrings)
    where fromString = Vocable2 . fromString

newtype Lexeme = Lexeme2 { lemma :: String } deriving (Eq,Ord)
instance IsString Lexeme -- to simplify typing the values (with OverloadedStrings)
    where fromString = Lexeme2 . fromString

	
showLitChar                :: Char -> ShowS
showLitChar c s | c > '\DEL' =  showChar '\\' (protectEsc isDec (shows (ord c)) s)

data Sex = Male | Female
data Person = Person {name :: String, age :: Int, sex :: Sex}
instance Show Person where
  show (Person _ a Male  ) | a < 20 = "A boy (" ++ show a ++ ")"
  show (Person _ a Female) | a < 20 = "A girl (" ++ show a ++ ")"
  show (Person n a _     )          = n

assert $ show (Person "村主崇行" 19 Male) == "A boy (19)"
assert $ show (Person "村主崇行" 20 Male) == "\26449\20027\23815\34892"
				  
:{
let myShow :: Show a => a -> String
    myShow x = go (show x) where
      go :: String -> String
      go [] = []
      go s@(x:xs) = case x of
          '\"' -> '\"' : str ++ "\"" ++ go rest
          '\'' -> '\'' : char : '\'' : go rest'
          _    -> x : go xs
        where
          (str :: String, rest):_ = reads s
          (char :: Char, rest'):_ = reads s
:}

:{
let myPrint :: Show a => a -> IO ()
    myPrint = putStrLn . myShow
:}
:set -interactive-print=myPrint

{-# LANGUAGE TypeSynonymInstances #-}
:set +t
read "123.456" :: Float
reads "5" :: [(Double, String)]
reads "6.8KΩ" :: [(Double, String)]

let d1 = [Just 5, Nothing, Nothing, Just 8, Just 9]::[Maybe Int]
d1
putStrLn (show d1)
writeFile "test" (show d1)
input <- readFile "test"
import System.IO (readFile)
input <- readFile "test"
let d2 = read input
d2
*** Exception: Prelude.read: no parse
input <- readFile "test"
input
let d2 = (read input)::[Maybe Int]
d2
putStrLn $ show [Left 5, Right "three", Left 0, Right "nine"]
