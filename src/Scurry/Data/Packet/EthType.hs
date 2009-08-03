module Scurry.Data.Packet.EthType (
    EthType(..),
) where

import Data.Word
import Foreign.Storable
import Foreign.Ptr
import Scurry.Network.Util

data EthType = ET_IP    -- 0800
             | ET_ARP   -- 0806
             | ET_IPX   -- 8037
             | ET_IPv6  -- 86DD
             | ET_LLDP  -- 88CC
             | ET_Other Word16
    deriving (Show,Eq)

instance Enum EthType where
    fromEnum e | e == ET_IP   = 0x0800
               | e == ET_ARP  = 0x0806
               | e == ET_IPX  = 0x8037
               | e == ET_IPv6 = 0x86DD
               | e == ET_LLDP = 0x88CC
               | otherwise    = case e of
                                    (ET_Other w) -> fromIntegral w
                                    _ -> error "Unknown EthType"

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
