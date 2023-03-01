module MyPrint (myPrint, myShow) where

myPrint :: Show a => a -> IO ()
myPrint = putStrLn . myShow
myShow :: Show a => a -> String
myShow x = con (show x) where
  con :: String -> String
  con [] = []
  con li@(x:xs) | x == '\"' = '\"':str++"\""++(con rest)
                | x == '\'' = '\'':char:'\'':(con rest')
                | otherwise = x:con xs where
                  (str,rest):_ = reads li
                  (char,rest'):_ = reads li