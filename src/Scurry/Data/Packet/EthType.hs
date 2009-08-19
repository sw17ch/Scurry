module Scurry.Data.Packet.EthType (
    EthType(..),
) where

import Foreign.Storable
import Foreign.Ptr
import Scurry.Network.Util
import Data.Binary

data EthType = ET_IP    -- 0800
             | ET_ARP   -- 0806
             | ET_IPX   -- 8037
             | ET_IPv6  -- 86DD
             | ET_LLDP  -- 88CC
             | ET_Other Word16
    deriving (Show,Eq)

instance Enum EthType where
    fromEnum ET_IP   = 0x0800
    fromEnum ET_ARP  = 0x0806
    fromEnum ET_IPX  = 0x8037
    fromEnum ET_IPv6 = 0x86DD
    fromEnum ET_LLDP = 0x88CC
    fromEnum (ET_Other w) = fromIntegral w

    toEnum 0x0800 = ET_IP
    toEnum 0x0806 = ET_ARP
    toEnum 0x8037 = ET_IPX
    toEnum 0x86DD = ET_IPv6
    toEnum 0x88CC = ET_LLDP
    toEnum w = ET_Other . fromIntegral $ w

instance Storable EthType where
    sizeOf _    = sizeOf (undefined :: Word16)
    alignment _ = 1
    peek p      = peek p' >>= (return . toEnum . fromIntegral . ntohs)
        where p' = castPtr p :: Ptr Word16
    poke p t    = poke p' (htons . fromIntegral . fromEnum $ t)
        where p' = castPtr p :: Ptr Word16

instance Binary EthType where
    get = do
        t <- get :: Get Word16
        return . toEnum . fromIntegral $ t
    put t = do
        let t'  = fromEnum t
            t'' = fromIntegral t' :: Word16
        put t''
