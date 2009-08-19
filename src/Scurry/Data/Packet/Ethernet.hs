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
    EthFrame(..),
) where

import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Ptr
import Data.Binary

import Data.List (intersperse)
import Data.ByteString.Lazy hiding (length, concat, map, intersperse,
                                    take, repeat, zipWith)
import qualified Data.ByteString.Lazy as B
import Text.Printf

import Scurry.Data.Packet.EthType

-- Read from a TAP device
newtype LocalPKT  = LocalPKT  ByteString deriving (Show)

-- Read from a Socket
newtype RemotePKT = RemotePKT ByteString deriving (Show)

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
    fromFrame :: a -> Ethernet
    toFrame   :: Ethernet -> a

------------------------
-- Explicit instances --
------------------------
instance EthFrame LocalPKT where
    -- fromFrame :: LocalPKT -> IO Ethernet
    fromFrame (LocalPKT p) = decode p

    -- toFrame :: Ethernet -> IO LocalPKT
    toFrame = LocalPKT . encode

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

instance Binary MACAddr where
    get = (sequence gs) >>= return . mkMACAddr
        where gs = take 6 $ repeat get
    put (MACAddr m) = sequence_ $ zipWith ($) (repeat put) m

instance Binary Ethernet where
    get = do
        d <- get
        s <- get
        t <- get
        return $ Ethernet {dst = d, src = s, etype = t }
    put e = do
        put $ dst   e
        put $ src   e
        put $ etype e
