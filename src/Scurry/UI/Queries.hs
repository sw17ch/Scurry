{-# LANGUAGE DeriveDataTypeable #-}
module Scurry.UI.Queries (
    handleQuery,
    UIState(..),
) where

import Data.Maybe
import Data.Word

import Text.JSON.Generic

import Control.Concurrent.MVar
import Network.Shed.Httpd
import Network.URI

import qualified Scurry.Scurry as S
import qualified Scurry.UI.Events as E
import Scurry.UI.Responses
import Scurry.Data.Network

data Query = State
           | BadMsg -- We didn't recognize this.
    deriving (Eq,Show)

handleQuery :: (MVar S.Scurry) -> Request -> IO (Either E.UIEvent Response)
handleQuery state req = do
    putStrLn $ unwords [show txid, show args]

    case txid of
       Nothing  -> return $ Right noTxId
       (Just t) -> case msg of
                       (Left e)    -> return $ Left (E.UIEvent t e)
                       (Right qry) -> qryToRsp state qry >>= (return . Right)
    where
        rawargs = queryToArguments $ uriQuery . reqURI $ req
        txid = lookup "txid" rawargs
        args = filter ((/= "txid") . fst) rawargs
        msgs = map pairToMsg args
        msg  = if 1 /= length msgs
               then Right BadMsg
               else head msgs

pairToMsg :: (String,String) -> Either E.EventCode Query
pairToMsg ("event","shutdown") = Left E.Shutdown
pairToMsg ("event","start")    = Left E.Start
pairToMsg ("query","state")    = Right State
pairToMsg _                    = Right BadMsg

qryToRsp :: MVar S.Scurry -> Query -> IO Response
qryToRsp s q | q == BadMsg = return badQuery
             | q == State  = mkState s
             | otherwise = error "Unable to convert query to response."

mkState :: MVar S.Scurry -> IO Response
mkState s = readMVar s >>= (return . jsonRsp . toState)

-- Object to represent things we want to send as
-- state to the user.
data UIState = UIState {
    vpnAddr  :: String,
    vpnMask  :: String,
    bindAddr :: String,
    bindPort :: Word16
} deriving (Data,Typeable)

toState :: S.Scurry -> UIState
toState s = UIState {
    vpnAddr = case fmap inet_ntoa (S.vpnAddr s) of
                (Just a) -> a
                Nothing -> ""
                ,
    vpnMask = case fmap inet_ntoa (S.vpnMask s) of
                (Just m) -> m
                Nothing -> ""
                ,
    bindAddr = inet_ntoa (S.bindAddr s),
    bindPort = unIPPort (S.bindPort s)
}
