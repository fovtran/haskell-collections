openUrl :: String -> MaybeT IO String
openUrl url = case parseURI url of
    Nothing -> fail ""
    Just u  -> liftIO (getResponseBody =<< simpleHTTP (mkRequest GET u))

css :: ArrowXml a => String -> a XmlTree XmlTree
css tag = multi (hasName tag)

get :: String -> IO (IOSArrow XmlTree (NTree XNode))
get url = do
  contents <- runMaybeT $ openUrl url
  return $ readString [withParseHTML yes, withWarnings no] (fromMaybe "" contents)

contents <- runMaybeT $ openUrl "http://example.com"

main = do
    page <- get "http://www.reddit.com/r/pics"

main = do
  url <- parseArgs
  doc <- get url
  imgs <- runX . images $ doc
  sequence_ $ map download imgs
