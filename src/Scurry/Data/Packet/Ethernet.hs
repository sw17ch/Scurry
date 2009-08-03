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
    TAPPacket,
    Frame(..),
) where

import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.ForeignPtr
import Data.Word
import Data.List
import Text.Printf

import Scurry.Network.Util
import Scurry.Data.Packet.EthType

data TAPPacket

-- Read from a TAP device
newtype LocalPKT  = LocalPKT  (ForeignPtr TAPPacket) deriving (Show)

-- Read from a Socket
newtype RemotePKT = RemotePKT (ForeignPtr TAPPacket) deriving (Show)

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
    ptr   :: ForeignPtr Ethernet,
    dst   :: MACAddr,
    src   :: MACAddr,
    etype :: EthType
} deriving (Show)

-------------
-- Classes --
-------------

-- |Provides methods from converting between Ethernet messages
-- and a distinct packet storage format.
class Frame a where
    fromFrame :: a -> IO Ethernet
    toFrame   :: Ethernet -> IO a

------------------------
-- Explicit instances --
------------------------
instance Frame LocalPKT where
    -- fromFrame :: LocalPKT -> IO Ethernet
    fromFrame (LocalPKT p) = do
        e <- withForeignPtr p $ peek . castPtr
        return (e {ptr = castForeignPtr p})

    -- toFrame :: Ethernet -> IO LocalPKT
    toFrame e = do p <- mallocForeignPtrBytes $ sizeOf (undefined :: Ethernet) 
                   withForeignPtr p $ \p' -> poke (castPtr p') e
                   return $ LocalPKT p

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
        return $ Ethernet { ptr = undefined, dst = d, src = s,
                            etype = t }
