{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Scurry.UI where

import Data.List
import Data.Maybe
import Control.Monad
import Control.Concurrent.MVar
import Control.Monad.STM

import Scurry.Scurry
import Scurry.Config
import Scurry.UI.Queries
import Scurry.UI.Events

import Text.JSON.Generic

import Network.Socket (inet_addr)
import Network.Shed.Httpd
import Network.URI

port :: Int
port = 24999

ui :: (MVar Scurry) -> (MVar UIEvent) -> (MVar UIResponse) -> IO ()
ui state events responses = do
    a <- inet_addr "127.0.0.1" 
    initServerBind port a (server state events responses)

logger :: Request -> Response -> IO ()
logger rq rs = putStrLn $ "UI: (" ++ show (resCode rs) ++ ") " ++ (uriPath . reqURI $ rq)

server :: (MVar Scurry) -> (MVar UIEvent) -> (MVar UIResponse) -> Request -> IO Response
server state events responses r@(Request {reqURI}) = do
    res <- case uriPath reqURI of
                "/sq" -> checkQuery
                "/"   -> normal indexFile
                other -> normal other
    logger r res >> return res
    where
        checkQuery = do
            (event,response) <- handleQuery state r
            writeIfEvent event
            return response

        writeIfEvent (UIEvent _ NoEvent) = return ()
        writeIfEvent event = do
            putStrLn "Writing..."
            putMVar events event
            putStrLn "...done"

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
