import Data.Aeson
import Network.HTTP

main = do
    src <- openURL "http://www.reddit.com/user/chrissalij/about.json"
    -- Json parsing code goes here

openURL url = getResponseBody =<< simpleHTTP (getRequest url)
