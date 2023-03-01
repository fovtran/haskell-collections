:t \x -> x == x^2
:t (\() -> "s")
:t (\(a ) -> "s")
:t (\(a) -> (+ a 1))

map (\(a, b) -> a + b) [(1,2)] 
map (\(Name first _) -> first) [Name "John" "Smith", Name "Jane" "Doe"] -- matches a "Name" data type and its first field
map (\(x:_) -> x) [[1,2,3], [4,5,6]] -- matches the head of a list

data Tree a = Leaf | Node (Tree a) a (Tree a)
data Bool = True | False
data () = ()  -- the left () is blue; the right () is red
data (a, b) = (a, b)

putStr :: String -> IO ()
put :: s -> State s ()
put :: s -> (State s) ()

main = do
  x <- putStrLn "Hello"
  case x of
    () -> putStrLn "The only value..."
