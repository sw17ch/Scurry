{-# LANGUAGE ForeignFunctionInterface,
             EmptyDataDecls,
             CPP #-}
module Scurry.FFI (
    start,
    finish,
    openTAP,
    closeTAP,
    bringUp,
    setMTU,
    setIP,
    setMask,
    
    getMAC,

    TAP,
    MACAddr,
    mkMACAddr,

    readTAP,
    writeTAP,

    test,
) where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Data.Word

-- A TAP device desciptor
newtype TAP = TAP (Ptr TAPDesc)
    deriving (Show)

-- Read from a TAP device
newtype LocalPKT = LocalPKT (ForeignPtr TAPPacket)
    deriving (Show)

-- Read from a Socket
newtype RemotePKT = RemotePKT (ForeignPtr TAPPacket)
    deriving (Show)

newtype MACAddr = MACAddr [Word8]
    deriving (Show)

mkMACAddr :: [Word8] -> MACAddr
mkMACAddr m = case length m of
                   6 -> MACAddr m
                   _ -> error "A MACAddr is 6 bytes! No more! No less!"

maxPktSize :: Int
maxPktSize = 1560
    
data TAPDesc
data TAPPacket

-- |Allocate a TAP resource
start :: IO TAP
start = init_tap_ffi >>= (return . TAP)

-- |Deallocate a TAP resource
finish :: TAP -> IO CInt
finish (TAP p) = finish_tap_ffi p

-- |Open the TAP device
openTAP :: TAP -> String -> IO CInt
openTAP (TAP p) n = withCString n (\s -> open_tap_ffi p s)

-- |Close the TAP device
closeTAP :: TAP -> IO CInt
closeTAP (TAP p) = close_tap_ffi p

-- |Bring up the TAP device
bringUp :: TAP -> IO CInt
bringUp (TAP p) = bring_up_tap_ffi p

-- |Set the MTU of the TAP device
setMTU :: TAP -> Int -> IO CInt
setMTU (TAP p) m = set_mtu_ffi p (fromIntegral m)

-- |Set the IPv4 address of the TAP device
setIP :: TAP -> Word32 -> IO CInt
setIP (TAP p) a = set_ip_ffi p (fromIntegral a)

-- |Set the network mask of the TAP device
setMask :: TAP -> Word32 -> IO CInt
setMask (TAP p) m = set_mask_ffi p (fromIntegral m)

-- |Get the MAC address assigned to the TAP device
getMAC :: TAP -> IO MACAddr
getMAC (TAP p) = allocaArray 6 g
    where g m = do get_mac_ffi p m
                   peekArray 6 m >>= (return . mkMACAddr . (map fromIntegral))

-- |Read a packet from the TAP device
readTAP :: TAP -> IO (CInt,LocalPKT)
readTAP (TAP p) = do
    pkt <- mallocForeignPtrBytes maxPktSize
    len <- withForeignPtr pkt $ \pkt' -> do
        read_tap_ffi p pkt' (fromIntegral maxPktSize)
    return (len,LocalPKT pkt)

-- |Write a packet to the TAP device
writeTAP :: TAP -> RemotePKT -> Int -> IO CInt
writeTAP (TAP p) (RemotePKT pkt) len = withForeignPtr pkt $ \pkt' -> do
    wlen <- write_tap_ffi p pkt' (fromIntegral len)
    return wlen
    

-- tap_desc_t * init_tap();
foreign import CALLCONV "help.h init_tap" init_tap_ffi :: IO (Ptr TAPDesc)

-- void finish_tap(tap_desc_t * td);
foreign import CALLCONV "help.h finish_tap" finish_tap_ffi   :: (Ptr TAPDesc) -> IO CInt

-- int32_t open_tap(tap_desc_t * td);
foreign import CALLCONV "help.h open_tap" open_tap_ffi :: (Ptr TAPDesc) -> CString -> IO CInt

-- int32_t close_tap(tap_desc_t * td);
foreign import CALLCONV "help.h close_tap" close_tap_ffi :: (Ptr TAPDesc) -> IO CInt

-- int32_t bring_up_tap(tap_desc_t * td);
foreign import CALLCONV "help.h bring_up_tap" bring_up_tap_ffi :: (Ptr TAPDesc) -> IO CInt

-- int32_t set_mtu(tap_desc_t * td, uint32_t mtu);
foreign import CALLCONV "help.h set_mtu" set_mtu_ffi :: (Ptr TAPDesc) -> CUInt -> IO CInt

-- int32_t set_ip(tap_desc_t * td, uint32_t ip);
foreign import CALLCONV "help.h set_ip" set_ip_ffi :: (Ptr TAPDesc) -> CUInt -> IO CInt

-- int32_t set_mask(tap_desc_t * td, uint32_t mask);
foreign import CALLCONV "help.h set_mask" set_mask_ffi :: (Ptr TAPDesc) -> CUInt -> IO CInt

-- int32_t get_mac(tap_desc_t * td, MACAddr mac);
foreign import CALLCONV "help.h get_mac" get_mac_ffi :: (Ptr TAPDesc) -> (Ptr CUChar) -> IO CInt

-- int32_t read_tap(tap_desc_t * td, int8_t * buf, int32_t len)
foreign import CALLCONV "help.h read_tap" read_tap_ffi :: (Ptr TAPDesc) -> (Ptr TAPPacket) -> CInt -> IO CInt

-- int32_t write_tap(tap_desc_t * td, const int8_t * buf, int32_t len)
foreign import CALLCONV "help.h write_tap" write_tap_ffi :: (Ptr TAPDesc) -> (Ptr TAPPacket) -> CInt -> IO CInt

test :: IO ()
test = do
    d <- start
    openTAP d "scurry0"
    setMTU d 1200
    setIP d 0x0A0A0A0A
    setMask d 0x00FFFFFF
    bringUp d
    m <- getMAC d

    (l,p) <- readTAP d

    print l
    print p
    print m

    closeTAP d
    finish d

    putStrLn "Done"
