
module Scurry.Data.Packet (
    module Scurry.Data.Packet.Ethernet,
    module Scurry.Data.Packet.IPv4,
    parsePkt,
    encodePkt,
) where

import Scurry.Data.Packet.Ethernet
import Scurry.Data.Packet.IPv4

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

data Frame = Frame {
    eth  :: Ethernet,
    pl   :: EthPayload
} deriving (Show)

data EthPayload = PL_IPv4  IPv4
                | PL_Other B.ByteString
    deriving (Show)

instance Binary Frame where
    get = do
        e <- get
        p <- case etype e of
                  ET_IP -> get >>= (return . PL_IPv4)
                  _     -> getRemainingStrict >>= (return . PL_Other)
        return $ Frame { eth = e, pl = p }
    put f = do
        put $ eth f
        case pl f of
            (PL_IPv4  i) -> put i
            (PL_Other o) -> putByteString o

parsePkt :: B.ByteString -> Frame
parsePkt b = decode $ L.fromChunks [b]

encodePkt :: Frame -> B.ByteString
encodePkt = B.concat . L.toChunks . encode

getRemainingStrict :: Get B.ByteString
getRemainingStrict = remaining >>= (getBytes . fromIntegral)
