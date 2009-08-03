{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scurry.NetworkData (
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

newtype MACAddr = MACAddr [Word8]
    deriving (Show)

mkMACAddr :: [Word8] -> MACAddr
mkMACAddr m = case length m of
                   6 -> MACAddr m
                   _ -> error "A MACAddr is 6 bytes! No more! No less!"

unMACAddr :: MACAddr -> [Word8]
unMACAddr (MACAddr m) = m

newtype EthType = EthType Word16
    deriving (Show,Num,Eq,Storable)

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
        return $ Ethernet { dst = d, src = s, etype = t }

