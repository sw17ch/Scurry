
module Scurry.Data.Packet (
    module Scurry.Data.Packet.Ethernet,
    module Scurry.Data.Packet.IPv4,
    parsePkt,
    encodePkt,
    Packet(..),
) where

import Scurry.Data.Packet.Ethernet
import Scurry.Data.Packet.IPv4

import Data.ByteString.Lazy
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

data Frame = Frame {
    eth  :: Ethernet,
    pl   :: EthPayload
} deriving (Show)

data EthPayload = PL_IPv4  IPv4
                | PL_Other ByteString
    deriving (Show)

instance Binary Frame where
    get = do
        e <- get
        p <- case etype e of
                  ET_IP -> get >>= (return . PL_IPv4)
                  _     -> getRemainingLazyByteString >>= (return . PL_Other)
        return $ Frame { eth = e, pl = p }
    put f = do
        put $ eth f
        case pl f of
            (PL_IPv4  i) -> put i
            (PL_Other o) -> putLazyByteString o

parsePkt :: (Packet p) => p -> Frame
parsePkt = decode . unpacket

encodePkt :: (Packet p) => Frame -> p
encodePkt = mkpacket . encode

class Packet a where
    unpacket :: a -> ByteString
    mkpacket :: ByteString -> a

instance Packet LocalPKT where
    unpacket (LocalPKT p) = p
    mkpacket b = LocalPKT b

