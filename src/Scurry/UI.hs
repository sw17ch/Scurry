{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Scurry.UI where

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

loadUI :: IO FilePath -> IO String
loadUI f = f >>= readFile

server :: (MVar Scurry) -> Request -> IO Response
server mv r@(Request {reqURI}) = do
    logger r

    case uriPath reqURI of
        "/index"  -> loadUI uiIndex  >>= (return . htmlOK)
        "/jquery" -> loadUI uiJQuery >>= (return . javaScriptOK)
        "/style"  -> loadUI uiStyle  >>= (return . styleSheetOK)
        "/script" -> loadUI uiScript >>= (return . javaScriptOK)
        ('/':'c':'m':'d':_) -> handleCmd mv r
        _         -> return (htmlOK "Unknown page")

handleCmd :: (MVar Scurry) -> Request -> IO Response
handleCmd mv r@(Request {}) = do
    (readMVar mv) >>= (return . jsApplicationOK . encodeJSON)

-- Default response
defResponse :: Response
defResponse = Response {
    resCode = 200,
    resHeaders = [contentType "text/plain"],
    resBody = "Default Response"
}

-- Wrap string response as HTML
htmlOK :: String -> Response
htmlOK body = defResponse {
    resHeaders = [contentType "text/html"],
    resBody = body
}

-- Wrap string response as JavaScript
javaScriptOK :: String -> Response
javaScriptOK body = defResponse {
    resHeaders = [contentType "text/javascript"],
    resBody = body
}

-- Wrap string response as CSS
styleSheetOK :: String -> Response
styleSheetOK body = defResponse {
    resHeaders = [contentType "text/css"],
    resBody = body
}

-- Write string response as JavaScript Application
jsApplicationOK :: String -> Response
jsApplicationOK body = defResponse {
    resHeaders = [contentType "application/x-javascript"],
    resBody = body
}
