module AsyncExceptions.Sample where

import Data.ByteString.Char8 (pack)
import Control.Concurrent.Async
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Network.HTTP
import Text.Printf


getURL :: String -> IO ByteString
getURL url =
  simpleHTTP (getRequest url) >>= fmap pack . getResponseBody

sites = [ "http://www.google.com"
        , "http://www.bing.com"
        , "http://www.yahoo.com"
        , "http://www.wikipedia.com/wiki/Spade"
        , "http://www.wikipedia.com/wiki/Shovel"
        ]

main :: IO ()
main =
  let download url = (,) <$> (pure url)
                         <*> (getURL url)
  in do
    as <- mapM (async . download) sites
    (_, (url, r)) <- waitAny as
    printf "%s was first (%d bytes)\n" url (B.length r)
    mapM_ wait as


