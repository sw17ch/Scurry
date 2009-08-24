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

import Scurry.Crypto
import Scurry.Scurry

data NetMsg = Reject
            | Join   { key :: ScurryPubKey }
            | Accept { key :: ScurryPubKey }
            | EncMsg { msg :: B.ByteString }
    deriving (Show)

netDbg :: String -> IO ()
netDbg = putStrLn

delayOneSec :: IO ()
delayOneSec = threadDelay (1 * 1000000)

-- | Kicks off read/write tasks on a socket
netTask :: MVar Scurry -> MVar () -> IO ()
netTask s _ = do
    (r,w) <- spawn
    catch (forever idle) $ \e -> do
        let err = show (e :: SomeException)
        netDbg $ "netTask: " ++ err
        throwTo r e
        throwTo w e
    where
        spawn = do
            r <- forkIO $ netRead  s
            w <- forkIO $ netWrite s
            return (r,w)
        idle = do
            delayOneSec
            netDbg "netTask: tick"

netRead :: MVar Scurry -> IO ()
netRead _ = genericCatch "netRead" task
    where
        task = forever $ idle
        idle = do
            delayOneSec
            netDbg "netRead: tick"

netWrite :: MVar Scurry -> IO ()
netWrite _ = genericCatch "netWrite" task
    where
        task = forever $ idle
        idle = do
            delayOneSec
            netDbg "netWrite: tick"

genericCatch :: String -> IO () -> IO ()
genericCatch ident a = do
    catch a (gc ident)
    where
        gc i e = do
            let err = show (e :: SomeException)
            netDbg $ i ++ ": " ++ err
