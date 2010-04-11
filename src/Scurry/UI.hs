{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Scurry.UI (
    ui,
) where

import Data.List
import Data.Maybe
import Control.Monad
import Control.Concurrent.MVar

import Scurry.Scurry
import Scurry.Config
import Scurry.UI.Queries
import Scurry.UI.Events
import Scurry.UI.Responses

import Network.Socket (inet_addr)
import Network.Shed.Httpd
import Network.URI

port :: Int
port = 24999

ui :: MVar Scurry -> MVar UIEvent -> MVar UIResponse -> IO ()
ui state events responses = do
    a <- inet_addr "127.0.0.1" 
    initServerBind port a (server state events responses)

logger :: Request -> Response -> IO ()
logger rq rs = putStrLn $
    "UI: (" ++ show (resCode rs) ++ ") " ++ (uriPath . reqURI $ rq)

server :: MVar Scurry
       -> MVar UIEvent
       -> MVar UIResponse
       -> Request
       -> IO Response
server state events responses r@(Request {reqURI}) = do
    res <- case uriPath reqURI of
                "/sq" -> checkQuery
                "/"   -> normal indexFile
                other -> normal other
    logger r res >> return res
    where
        checkQuery = do
            eOrR <- handleQuery state r
            r'   <- case eOrR of
                         (Right rsp)  -> return rsp
                         (Left event) -> do
                            putMVar events event
                            mr <- takeMVar responses
                            return $ jsonRsp mr

            return $ r'

        normal fn = do
            f <- uiFile fn
            case f of
                (Just n) -> fileResponse n
                Nothing -> return notFound
