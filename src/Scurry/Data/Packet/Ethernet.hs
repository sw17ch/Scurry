{-# LANGUAGE EmptyDataDecls,
             GeneralizedNewtypeDeriving #-}
module Scurry.Data.Packet.Ethernet (
    MACAddr,
    mkMACAddr,
    unMACAddr,

    Ethernet(src,dst,etype),
    EthType(..),

    LocalPKT(..),
    RemotePKT(..),
    EthernetFrame,
    EthFrame(..),
    Framed(..),
    Packet(..),
) where

import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.ForeignPtr
import Data.Word
import Data.List
import Text.Printf

import Scurry.Data.Packet.EthType

data EthernetFrame

-- Read from a TAP device
newtype LocalPKT  = LocalPKT  (ForeignPtr EthernetFrame) deriving (Show)

-- Read from a Socket
newtype RemotePKT = RemotePKT (ForeignPtr EthernetFrame) deriving (Show)

-- | A MACAddr is a hardware address 48 bits long.
newtype MACAddr = MACAddr [Word8]

-- | Expects a single argument -- a list 6 Word8's long
mkMACAddr :: [Word8] -> MACAddr
mkMACAddr m = case length m of
                   6 -> MACAddr m
                   _ -> error "A MACAddr is 6 bytes! No more! No less!"

-- | Get the list behind the MACAddr
unMACAddr :: MACAddr -> [Word8]
unMACAddr (MACAddr m) = m

-- | Ethernet is an ethernet header. This consists of the first 92 bytes of
-- any ethernet packet.
data Ethernet = Ethernet {
    dst   :: MACAddr,
    src   :: MACAddr,
    etype :: EthType
} deriving (Show)

-------------
-- Classes --
-------------

-- |Provides methods from converting between Ethernet messages
-- and a distinct packet storage format.
class EthFrame a where
    fromFrame :: a -> IO Ethernet
    toFrame   :: Ethernet -> IO a

-- |Something that is Framed can be possibly be pulled out
-- of an ethernet frame.
class Framed a where
    unframe :: (Packet p) => p -> IO (Maybe a)

class Packet a where
    unpacket :: a -> ForeignPtr EthernetFrame

------------------------
-- Explicit instances --
------------------------
instance EthFrame LocalPKT where
    -- fromFrame :: LocalPKT -> IO Ethernet
    fromFrame (LocalPKT p) = withForeignPtr p $ peek . castPtr

    -- toFrame :: Ethernet -> IO LocalPKT
    toFrame e = do p <- mallocForeignPtrBytes $ sizeOf (undefined :: Ethernet) 
                   withForeignPtr p $ \p' -> poke (castPtr p') e
                   return $ LocalPKT p

instance Packet LocalPKT where
    unpacket (LocalPKT p) = p

instance Framed Ethernet where
    unframe p = withForeignPtr (unpacket p) (peek . castPtr) >>= (return . Just)

instance Show MACAddr where
    show (MACAddr m) = concat $ intersperse ":" $ map (printf "%02X") m

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
        return $ Ethernet { dst = d, src = s, etype = t }
