module Scurry.UI.Responses (
    decideContent,

    fileResponse,
    jsonRsp,

    notFound,
    unimplemented,
    badQuery,
    noTxId,
) where

import Data.Data()
import Data.List
import Network.Shed.Httpd
import Text.JSON.Generic

decideContent :: FilePath -> String
decideContent n | ".js"   `isSuffixOf` n = "text/javascript"
                | ".txt"  `isSuffixOf` n = "text/plain"
                | ".css"  `isSuffixOf` n = "text/css"
                | ".html" `isSuffixOf` n = "text/html"
                | ".json" `isSuffixOf` n = "application/x-javascript"
                | otherwise = "text/plain"

fileResponse :: FilePath -> IO Response
fileResponse p = do
    b <- readFile p

    return $ Response {
        resCode = 200,
        resHeaders = [contentType (decideContent p)],
        resBody = b
    }

jsonRsp :: (Data a, Typeable a) => a -> Response
jsonRsp a = Response {
    resCode = 200,
    resHeaders = [contentType "application/x-javascript"],
    resBody = encodeJSON a
}

unimplemented :: Response
unimplemented = Response {
    resCode = 200,
    resHeaders = [contentType "text/plain"],
    resBody = "Unimplemented."
}

badQuery :: Response
badQuery = Response {
    resCode = 200,
    resHeaders = [contentType "text/plain"],
    resBody = "Unknown query."
}

notFound :: Response
notFound = Response {
    resCode = 404,
    resHeaders = [contentType "text/plain"],
    resBody = "File not found."
}

noTxId :: Response
noTxId = Response {
    resCode = 422,
    resHeaders = [contentType "text/plain"],
    resBody = "No transaction ID provided."
}
