{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scurry.Data.Packet (
    MACAddr,
    mkMACAddr,
    unMACAddr,

    EthType(..),

    Ethernet(..),
) where

import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Ptr

import Data.Word
import Data.List
import Data.Bits
import Text.Printf

newtype MACAddr = MACAddr [Word8]

instance Show MACAddr where
    show (MACAddr m) = concat $ intersperse ":" $ map (printf "%02X") m

mkMACAddr :: [Word8] -> MACAddr
mkMACAddr m = case length m of
                   6 -> MACAddr m
                   _ -> error "A MACAddr is 6 bytes! No more! No less!"

unMACAddr :: MACAddr -> [Word8]
unMACAddr (MACAddr m) = m

newtype EthType = EthType { unEthType :: Word16 }
    deriving (Num,Eq,Storable)

instance Show EthType where
    show (EthType t) = printf "%04X" t

data Ethernet = Ethernet {
    dst   :: MACAddr,
    src   :: MACAddr,
    etype :: EthType
} deriving (Show)

-- Instances

sizeMACAddr :: Int
sizeMACAddr = sizeOf m
    where m = undefined :: MACAddr

instance Storable MACAddr where
    sizeOf _    = 6
    alignment _ = 1
    peek p      = peekArray sizeMACAddr (castPtr p) >>= (return . mkMACAddr)
    poke p m    = pokeArray (castPtr p) (unMACAddr m)

instance Storable Ethernet where
    sizeOf _    = (6 + 6 + 2)
    alignment _ = 1
    peek p      = do
        d <- peek $ castPtr p
        s <- peek $ castPtr $ p `plusPtr` sizeMACAddr
        t <- peek $ castPtr $ p `plusPtr` sizeMACAddr `plusPtr` sizeMACAddr
        return $ Ethernet { dst = d, src = s,
                            etype = EthType . ntohs . unEthType $ t }

-- ntohl :: Word32 -> Word32
-- htonl :: Word32 -> Word32
ntohs :: Word16 -> Word16
ntohs w = let b0 = shiftR (0xFF00 .&. w) 8
              b1 = shiftL (0x00FF .&. w) 8
          in  b0 .|. b1
          
htons :: Word16 -> Word16
htons = ntohs
