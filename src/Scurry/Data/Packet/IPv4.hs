{-# LANGUAGE EmptyDataDecls,
             GeneralizedNewtypeDeriving #-}

module Scurry.Data.Packet.IPv4 (
    IPv4(..),    
) where

import Data.Word

data IPv4 = IPv4 {
    version  :: IPv4_Version,
    hdrLen   :: IPv4_HdrLen,
    diffServ :: IPv4_DiffServ,
    totalLen :: IPv4_TotalLen,
    ident    :: IPv4_Ident,
    flags    :: IPv4_Flags,
    fragOff  :: IPv4_FragOff,
    ttl      :: IPv4_TTL,
    protocol :: IPv4_Protocol,
    hdrChck  :: IPv4_HdrChck,
    srcAddr  :: IPv4_SrcAddr,
    dstAddr  :: IPv4_DstAddr,
    options  :: Maybe IPv4_Options
} deriving (Show)

newtype IPv4_Version  = IPv4_Version  Word8  deriving (Show)
newtype IPv4_HdrLen   = IPv4_HdrLen   Word8  deriving (Show)
newtype IPv4_DiffServ = IPv4_DiffServ Word8  deriving (Show)
newtype IPv4_TotalLen = IPv4_TotalLen Word16 deriving (Show)
newtype IPv4_Ident    = IPv4_Ident    Word16 deriving (Show)
newtype IPv4_Flags    = IPv4_Flags    Word8  deriving (Show)
newtype IPv4_FragOff  = IPv4_FragOff  Word16 deriving (Show)
newtype IPv4_TTL      = IPv4_TTL      Word8  deriving (Show)
newtype IPv4_Protocol = IPv4_Protocol Word8  deriving (Show)
newtype IPv4_HdrChck  = IPv4_HdrChck  Word16 deriving (Show)
newtype IPv4_SrcAddr  = IPv4_SrcAddr  Word32 deriving (Show)
newtype IPv4_DstAddr  = IPv4_DstAddr  Word32 deriving (Show)
newtype IPv4_Options  = IPv4_Options  Word32 deriving (Show)
