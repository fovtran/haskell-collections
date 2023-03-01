module Eval() where

import Prelude hiding (lex)
import Foreign.C
import Foreign
import Control.Applicative
import Text.ParserCombinators.ReadP
import Text.Read.Lex

foreign export ccall "eval" c_eval :: CString -> Ptr CInt -> IO (Ptr CInt)

c_eval s r = do
    cs <- peekCAString s
    case hs_eval cs of
        Nothing -> return nullPtr
        Just x -> do
            poke r x
            return r

hs_eval :: String -> Maybe CInt
hs_eval inp = case readP_to_S expr inp of
    (a,_) : _ -> Just a
    [] -> Nothing

expr = addition <* expect EOF

addition = chainl1 multiplication add
  where
    add = expect (Symbol "+") >> return (+)

multiplication = chainl1 atom mul
  where
    mul = expect (Symbol "*") >> return (*)

atom = number <|> between lp rp addition

number = do
    Number n <- lex
    case numberToInteger n of
        Just i -> return (fromIntegral i)
        Nothing -> pfail

lp = expect (Punc "(")
rp = expect (Punc ")")
