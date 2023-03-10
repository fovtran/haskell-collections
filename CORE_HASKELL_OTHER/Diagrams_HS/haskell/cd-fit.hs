-- Taken from 'cd-fit-2-1.hs'
import Text.ParserCombinators.Parsec

-- parseInput parses output of "du -sb", which consists of many lines,
-- each of which describes single directory
parseInput =
  do dirs <- many dirAndSize
     eof :: Parser ()
     return dirs

-- Datatype Dir holds information about single directory - its size and name
data Dir = Dir Int String deriving Show

-- `dirAndSize` parses information about single directory, which is:
-- a size in bytes (number), some spaces, then directory name, which extends till newline
dirAndSize =
  do size <- many1 digit
     spaces
     dir_name <- anyChar `manyTill` newline
     return (Dir (read size) dir_name)
