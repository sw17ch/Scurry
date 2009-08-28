module Scurry.NetTask (
    NetMsg(..),
    netTask,
) where

import Prelude hiding (catch)

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import qualified Data.ByteString.Lazy as B

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString


import Scurry.Crypto
import Scurry.Data.Network
import Scurry.Scurry

data NetMsg = Reject
            | Join   { key :: ScurryPubKey }
            | Accept { key :: ScurryPubKey }
            | EncMsg { msg :: B.ByteString }
    deriving (Show)

netDbg :: String -> IO ()
-- netDbg = putStrLn
netDbg _ = do return ()

delayOneSec :: IO ()
delayOneSec = threadDelay (1 * 1000000)

-- | Kicks off read/write tasks on a socket
netTask :: MVar Scurry -> MVar () -> IO ()
netTask s _ = do
    sck   <- (readMVar s) >>= prepSocket 
    (r,w) <- spawn sck
    catch (forever idle) $ \e -> do
        let err = show (e :: SomeException)
        netDbg $ "netTask: " ++ err
        throwTo r e
        throwTo w e
    where
        spawn sck = do
            r <- forkIO $ netRead  s sck
            w <- forkIO $ netWrite s sck
            return (r,w)
        idle = do
            delayOneSec
            netDbg "netTask: tick"

netRead :: MVar Scurry -> Socket -> IO ()
netRead _ sck = genericCatch "netRead" (forever go)
    where
        go = netDbg "netRead: read" >> recvFrom sck 1600

netWrite :: MVar Scurry -> Socket -> IO ()
netWrite _ sck = genericCatch "netWrite" task
    where
        task = forever $ idle
        idle = do
            delayOneSec
            netDbg "netWrite: tick"

prepSocket :: Scurry -> IO Socket
prepSocket c = do
    s <- socket AF_INET Datagram defaultProtocol
    setSocketOption s Broadcast 4
    bindSocket s sockAddr
    return s
    where
        sockAddr = SockAddrInet (fromIntegral . unIPPort . bindPort $ c)
                                (unIPV4Addr . bindAddr $ c)

genericCatch :: String -> IO () -> IO ()
genericCatch ident a = do
    catch a (gc ident)
    where
        gc i e = do
            let err = show (e :: SomeException)
            netDbg $ i ++ ": " ++ err
