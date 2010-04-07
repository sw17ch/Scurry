module Scurry.UI.Queries (
    handleQuery,
) where

import Data.Maybe

import Control.Concurrent.MVar
import Network.Shed.Httpd
import Network.URI

import Scurry.Scurry
import qualified Scurry.UI.Events as E

data Query = Shutdown
           | BadQuery -- We didn't recognize this.
    deriving (Show)

badQuery :: Response
badQuery = Response {
    resCode = 404,
    resHeaders = [contentType "text/plain"],
    resBody = "Unknown query."
}

noTxId :: Response
noTxId = Response {
    resCode = 422,
    resHeaders = [contentType "text/plain"],
    resBody = "No transaction ID provided."
}

norm :: (Show a) => a -> Response
norm a = Response {
    resCode = 200,
    resHeaders = [contentType "text/plain"],
    resBody = show a
}

queryToEvent :: Query -> E.EventCode
queryToEvent Shutdown = E.Shutdown
queryToEvent BadQuery = E.NoEvent

handleQuery :: (MVar Scurry) -> Request -> IO (E.UIEvent,Response)
handleQuery state req = do
    putStrLn $ unwords [show txid, show args]

    if isJust txid
        then return (E.UIEvent (fromJust txid) event, norm queries)
        else return (E.UIEvent (fromJust txid) event, noTxId)
    where
        rawargs = queryToArguments $ uriQuery . reqURI $ req
        txid = lookup "txid" rawargs
        args = filter ((/= "txid") . fst) rawargs
        queries = map pairToQuery args

        event = let es = map queryToEvent queries
                in if 1 /= (length es)
                    then E.NoEvent
                    else head es

pairToQuery :: (String,String) -> Query
pairToQuery ("event","shutdown") = Shutdown
pairToQuery _                    = BadQuery
