{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Scurry.UI where

import Data.List

import Scurry.Scurry
import Scurry.Config

import Control.Concurrent.MVar

import Text.JSON.Generic

import Network.Socket (inet_addr)
import Network.Shed.Httpd
import Network.URI

port :: Int
port = 24999

ui :: (MVar Scurry) -> (MVar ()) -> IO ()
ui mv shutdown = do
    a <- inet_addr "127.0.0.1" 
    initServerBind port a (server mv)

logger :: Request -> IO ()
logger r = print r

server :: (MVar Scurry) -> Request -> IO Response
server mv r@(Request {reqURI}) = do
    logger r

    case uriPath reqURI of
        "/" -> normal indexFile
        other -> normal other

    where
        normal fn = do
            f <- uiFile fn

            case f of
                (Just n) -> mkResponse n
                Nothing -> return badResponse

decideContent :: FilePath -> String
decideContent n | ".js"   `isSuffixOf` n = "text/javascript"
                | ".txt"  `isSuffixOf` n = "text/plain"
                | ".css"  `isSuffixOf` n = "text/css"
                | ".html" `isSuffixOf` n = "text/html"
                | ".json" `isSuffixOf` n = "application/x-javascript"
                | otherwise = "text/plain"

handleCmd :: (MVar Scurry) -> Request -> IO Response
handleCmd mv r@(Request {}) = do
    (readMVar mv) >>= (return . jsApplicationOK . encodeJSON)

mkResponse :: FilePath -> IO Response
mkResponse p = do
    b <- readFile p

    return $ Response {
        resCode = 200,
        resHeaders = [contentType (decideContent p)],
        resBody = b
    }

badResponse :: Response
badResponse = Response {
    resCode = 404,
    resHeaders = [contentType "text/plain"],
    resBody = "File not found."
}

jsApplicationOK :: String -> Response
jsApplicationOK body = Response {
    resCode = 200,
    resHeaders = [contentType "application/x-javascript"],
    resBody = body
}
