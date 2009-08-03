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
} deriving (Show,Data,Typeable)

defaultScurry :: Scurry
defaultScurry = Scurry {
    vpnAddr  = Nothing,
    vpnMask  = Nothing,
    bindAddr = read "0.0.0.0",
    bindPort = read "24999",
    peerName = "Scurry Peer"
}
