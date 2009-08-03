module Scurry.Network.Util (
    ntohl, ntohs,
    htonl, htons,
) where

import Data.Word
import Data.Bits

-- | Network to Host Long: Convert from network to host byte order
-- on a little endian host.
ntohl :: Word32 -> Word32
ntohl w = let b0 = shiftR (0xFF000000 .&. w) 24
              b1 = shiftR (0x00FF0000 .&. w) 8
              b2 = shiftL (0x0000FF00 .&. w) 8
              b3 = shiftL (0x000000FF .&. w) 24
          in  b0 .|. b1 .|. b2 .|. b3

-- | Host to Network Long: Convert from host to network byte order
-- on a little endian host.
htonl :: Word32 -> Word32
htonl = ntohl

-- | Network to Host Short: Convert from network to host byte order
-- on a little endian host.
ntohs :: Word16 -> Word16
ntohs w = let b0 = shiftR (0xFF00 .&. w) 8
              b1 = shiftL (0x00FF .&. w) 8
          in  b0 .|. b1
          
-- | Host to Network Short: Convert from host to network byte order
-- on a little endian host.
htons :: Word16 -> Word16
htons = ntohs
