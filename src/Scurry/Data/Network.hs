{-# LANGUAGE DeriveDataTypeable, CPP, ForeignFunctionInterface #-}
module Scurry.Data.Network (
    IPV4Addr(unIPV4Addr),
    IPPort(..),
    VPNAddr,
    HWAddr,
    mkHWAddr,
    inet_ntoa,
    inet_addr,

    withSocketsDo, -- Network.Socket
) where

import Data.Data
import Data.Word

import Control.Monad
import System.IO.Unsafe (unsafePerformIO)

import qualified Network.Socket as INET (inet_addr,
                                         inet_ntoa)
import Network.Socket hiding (send, sendTo,
                              recv, recvFrom,
                              PortNum, -- WhyTF is this exposed
                              inet_addr, inet_ntoa)
-- import Network.Socket.ByteString

newtype IPV4Addr = MkIPV4Addr { unIPV4Addr :: HostAddress } deriving (Data,Typeable)
newtype IPPort   = MkIPPort   { unIPPort :: Word16 }        deriving (Data,Typeable)
newtype VPNAddr  = MkVPNAddr  IPV4Addr                      deriving (Data,Typeable)

newtype HWAddr   = MkHWAddr [Word8]                         deriving (Data,Typeable)

mkHWAddr :: [Word8] -> Maybe HWAddr
mkHWAddr adr = if length adr == 8
                then Just . MkHWAddr $ adr
                else Nothing

inet_addr :: String -> Maybe IPV4Addr
inet_addr = unsafePerformIO . catchToMaybe . mk
    where mk v = liftM MkIPV4Addr (INET.inet_addr v)

inet_ntoa :: IPV4Addr -> Maybe String
inet_ntoa (MkIPV4Addr a) = unsafePerformIO unmk
    where unmk = catchToMaybe . INET.inet_ntoa $ a

catchToMaybe :: (IO a) -> IO (Maybe a)
catchToMaybe a = catch (liftM Just a) (\_ -> return Nothing)

-- Read instances! Note, these suck and need to be fixed.
instance Read IPV4Addr where
    readsPrec _ r = case a of
                         (Just a') -> [(a',"")]
                         Nothing   -> error "IPV4Address: no parse!"
        where a = inet_addr r

instance Read IPPort where
    readsPrec _ r = [(p',"")]
        where p  = read r :: Word16
              p' = MkIPPort . fromIntegral $ p

instance Read VPNAddr where
    readsPrec _ r = [(MkVPNAddr v,"")]
        where v = read r

instance Show IPV4Addr where
    show a = case inet_ntoa a of
                  Just s -> s
                  Nothing -> error "This should never happen evar! O_o"

instance Show VPNAddr where
    show (MkVPNAddr a)= case inet_ntoa a of
                           Just s -> s
                           Nothing -> error "This should never happen evar! O_o"

instance Show IPPort where
    show (MkIPPort p) = show p
