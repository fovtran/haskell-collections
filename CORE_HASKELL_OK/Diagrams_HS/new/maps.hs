 {- A quick note for the over-eager refactorers out there: This is (somewhat)
    intentionally ugly. It doesn't use the State monad to hold the DB because it
    hasn't been introduced yet. Perhaps we could use this as an example of How
    Monads Improve Things? -}
 
 module PassDB where
 
 import qualified Data.Map as M
 import System.Exit
 
 type UserName = String
 type Password = String
 type PassDB   = M.Map UserName Password 
   -- PassBD is a map from usernames to passwords
 
 -- | Ask the user for a username and new password, and return the new PassDB
 changePass :: PassDB -> IO PassDB
 changePass db = do
   putStrLn "Enter a username and new password to change."
   putStr "Username: "
   un <- getLine
   putStrLn "New password: "
   pw <- getLine
   if un `M.member` db               -- if un is one of the keys of the map
     then return $ M.insert un pw db -- then update the value with the new password
     else do putStrLn $ "Can't find username '" ++ un ++ "' in the database."
             return db
 
 -- | Ask the user for a username, whose password will be displayed.
 viewPass :: PassDB -> IO ()
 viewPass db = do
   putStrLn "Enter a username, whose password will be displayed."
   putStr "Username: "
   un <- getLine
   putStrLn $ case M.lookup un db of
                Nothing -> "Can't find username '" ++ un ++ "' in the database."
                Just pw -> pw
 
 -- | The main loop for interacting with the user. 
 mainLoop :: PassDB -> IO PassDB
 mainLoop db = do
   putStr "Command [cvq]: "
   c <- getChar
   putStr "\n"
   -- See what they want us to do. If they chose a command other than 'q', then
   -- recurse (i.e. ask the user for the next command). We use the Maybe datatype
   -- to indicate whether to recurse or not: 'Just db' means do recurse, and in
   -- running the command, the old datbase changed to db. 'Nothing' means don't
   -- recurse.
   db' <- case c of
            'c' -> fmap Just $ changePass db
            'v' -> do viewPass db; return (Just db)
            'q' -> return Nothing
            _   -> do putStrLn $ "Not a recognised command, '" ++ [c] ++ "'."
                      return (Just db)
   maybe (return db) mainLoop db'
 
 -- | Parse the file we've just read in, by converting it to a list of lines,
 --   then folding down this list, starting with an empty map and adding the
 --   username and password for each line at each stage.
 parseMap :: String -> PassDB
 parseMap = foldr parseLine M.empty . lines
     where parseLine ln map = 
             let [un, pw] = words ln
             in M.insert un pw map
 
 -- | Convert our database to the format we store in the file by first converting
 --   it to a list of pairs, then mapping over this list to put a space between
 --   the username and password
 showMap :: PassDB -> String
 showMap = unlines . map (\(un, pw) -> un ++ " " ++ pw) . M.toAscList
 
 main :: IO ()
 main = do
   putStrLn $ "Welcome to PassDB. Enter a command: (c)hange a password, " ++
              "(v)iew a password or (q)uit."
   dbFile <- readFile "/etc/passdb"
   db'    <- mainLoop (parseMap dbFile)
   writeFile "/etc/passdb" (showMap db')