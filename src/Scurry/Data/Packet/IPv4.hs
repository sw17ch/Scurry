{-# LANGUAGE EmptyDataDecls,
             GeneralizedNewtypeDeriving #-}

module Scurry.Data.Packet.IPv4 (
    IPv4(..),
    IPv4_Version(..),
    IPv4_HdrLen(..),
    IPv4_DiffServ(..),
    IPv4_TotalLen(..),
    IPv4_Ident(..),
    IPv4_Flags(..),
    IPv4_FragOff(..),
    IPv4_TTL(..),
    IPv4_Protocol(..),
    IPv4_HdrChck(..),
    IPv4_SrcAddr(..),
    IPv4_DstAddr(..),
    IPv4_Options(..),
) where

import Data.Word
import Data.Bits
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Ptr

import Scurry.Data.Packet.Ethernet
import Scurry.Network.Util

data IPv4 = IPv4 {
    version  :: IPv4_Version ,
    hdrLen   :: IPv4_HdrLen,
    diffServ :: IPv4_DiffServ,
    totalLen :: IPv4_TotalLen,
    ident    :: IPv4_Ident,
    flags    :: IPv4_Flags,
    fragOff  :: IPv4_FragOff ,
    ttl      :: IPv4_TTL,
    protocol :: IPv4_Protocol,
    hdrChck  :: IPv4_HdrChck,
    srcAddr  :: IPv4_SrcAddr,
    dstAddr  :: IPv4_DstAddr,
    options  :: Maybe IPv4_Options
} deriving (Show)

newtype IPv4_Version  = IPv4_Version  { unVersion  :: Word8  } deriving (Show)
newtype IPv4_HdrLen   = IPv4_HdrLen   { unHdrLen   :: Word8  } deriving (Show)
newtype IPv4_DiffServ = IPv4_DiffServ { unDiffServ :: Word8  } deriving (Show)
newtype IPv4_TotalLen = IPv4_TotalLen { unTotalLen :: Word16 } deriving (Show)
newtype IPv4_Ident    = IPv4_Ident    { unIdent    :: Word16 } deriving (Show)
newtype IPv4_Flags    = IPv4_Flags    { unFlags    :: Word8  } deriving (Show)
newtype IPv4_FragOff  = IPv4_FragOff  { unFragOff  :: Word16 } deriving (Show)
newtype IPv4_TTL      = IPv4_TTL      { unTTL      :: Word8  } deriving (Show)
newtype IPv4_Protocol = IPv4_Protocol { unProtocol :: Word8  } deriving (Show)
newtype IPv4_HdrChck  = IPv4_HdrChck  { unHdrChck  :: Word16 } deriving (Show)
newtype IPv4_SrcAddr  = IPv4_SrcAddr  { unSrcAddr  :: Word32 } deriving (Show)
newtype IPv4_DstAddr  = IPv4_DstAddr  { unDstAddr  :: Word32 } deriving (Show)
newtype IPv4_Options  = IPv4_Options  { unOptions  :: Word32 } deriving (Show)

instance Storable IPv4 where
    sizeOf _    = (6 * 32) -- With Options field
    alignment _ = 1
    peek p      = do
        let prepPtr ofst = peek $ castPtr $ p `plusPtr` ofst

        verhdr <- prepPtr 0
        dif    <- prepPtr 1
        tot    <- prepPtr 2
        ide    <- prepPtr 4
        flafra <- prepPtr 6
        tto    <- prepPtr 8
        pro    <- prepPtr 9
        hdc    <- prepPtr 10
        sra    <- prepPtr 12
        dsa    <- prepPtr 16
        let (ver,hdr) = splt_verhdr verhdr
        let (fla,fra) = splt_flafra $ ntohs flafra
        opt    <- case (4 * hdr) of
                       24 -> (prepPtr 20) >>=
                             (return . Just . IPv4_Options)
                       _  -> return Nothing

        return $ IPv4 { version  = IPv4_Version  ver,
                        hdrLen   = IPv4_HdrLen   hdr,
                        diffServ = IPv4_DiffServ dif,
                        totalLen = IPv4_TotalLen . ntohs $ tot,
                        ident    = IPv4_Ident . ntohs $ ide,
                        flags    = IPv4_Flags fla,
                        fragOff  = IPv4_FragOff . ntohs $ fra,
                        ttl      = IPv4_TTL tto,
                        protocol = IPv4_Protocol pro,
                        hdrChck  = IPv4_HdrChck . ntohs $ hdc,
                        srcAddr  = IPv4_SrcAddr . ntohl $ sra,
                        dstAddr  = IPv4_DstAddr . ntohl $ dsa,
                        options  = opt}

        where splt_verhdr :: Word8 -> (Word8,Word8)
              splt_verhdr verhdr = let v = 0xF0 .&. verhdr
                                       h = 0x0F .&. verhdr
                                   in  (shiftR v 4, h)
              splt_flafra :: Word16 -> (Word8,Word16)
              splt_flafra flafra = let fl = 0xE000 .&. flafra
                                       fr = 0x1FFF .&. flafra
                                   in (fromIntegral $ shiftR fl 13, fr)

instance Framed IPv4 where
    unframe p = do
        ehdr <- unframe p
        case ehdr of
            Nothing  -> return Nothing
            (Just e) -> case etype e of
                             ET_IP -> gptr (unpacket p) >>=
                                      dopeek            >>=
                                      (return . Just)
                             _     -> return Nothing
        where
            -- get the correct pointer
            seth = sizeOf (undefined :: Ethernet)
            gptr p' = withForeignPtr p' $ \p'' -> return $ p'' `plusPtr` seth
            dopeek = (peek . castPtr)

