{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Scurry.UI where

import Scurry.Scurry
import Scurry.Config

import Control.Concurrent.MVar

import Network.Socket (inet_addr)
import Network.Shed.Httpd
import Network.URI

port :: Int
port = 24999

defResponse :: Response
defResponse = Response {
    resCode = 200,
    resHeaders = [contentType "text/plain"],
    resBody = "Default Response"
}

htmlOK :: String -> Response
htmlOK body = defResponse {
    resHeaders = [contentType "text/html"],
    resBody = body
}

javaScriptOK :: String -> Response
javaScriptOK body = defResponse {
    resHeaders = [contentType "text/javascript"],
    resBody = body
}

styleSheetOK :: String -> Response
styleSheetOK body = defResponse {
    resHeaders = [contentType "text/css"],
    resBody = body
}

ui :: (MVar Scurry) -> IO ()
ui mv = do
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
handleCmd mv r@(Request {..}) = do
    return $ javaScriptOK "{\"hello\" : \"world\"}"
