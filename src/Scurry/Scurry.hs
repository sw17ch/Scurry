{-# LANGUAGE DeriveDataTypeable #-}
module Scurry.Scurry (
    Scurry(..),
    defaultScurry
) where

import Scurry.Data.Network

import Text.JSON.Generic

data Scurry = Scurry {
    vpnAddr  :: Maybe VPNAddr,
    vpnMask  :: Maybe IPV4Addr,
    bindAddr :: IPV4Addr,
    bindPort :: IPPort,
    peerName :: String
} deriving (Data,Typeable)

defaultScurry :: Scurry
defaultScurry = Scurry {
    vpnAddr  = Nothing,
    vpnMask  = Nothing,
    bindAddr = read "0.0.0.0",
    bindPort = read "24999",
    peerName = "Scurry Peer"
}

instance Show Scurry where
    show s = unwords [
            "vpnAddr:" ++ va,
            "vpmMask:" ++ vm,
            "bindAddr:" ++ ba,
            "bindPort:" ++ bp,
            "peerName:" ++ pn
        ]
        
        where
            va = case (vpnAddr s) of
                      (Just a) -> show a
                      Nothing  -> "Unassigned"
            vm = case (vpnMask s) of
                      (Just m) -> show m
                      Nothing  -> "Unassigned"
            ba = show $ bindAddr s
            bp = show $ bindPort s
            pn = peerName s 
