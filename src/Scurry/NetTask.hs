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
import Scurry.Exception
import Scurry.Scurry

data NetMsg = Reject
            | Join   { key :: ScurryPubKey }
            | Accept { key :: ScurryPubKey }
            | EncMsg { msg :: B.ByteString }
    deriving (Show)

delayOneSec :: IO ()
delayOneSec = threadDelay (1 * 1000000)

-- | Kicks off read/write tasks on a socket
netTask :: MVar Scurry -> MVar () -> IO ()
netTask s c = do
    (r,w) <- spawn
    catch (forever idle) $ \e -> do
        let err = show (e :: SomeException)
        putStrLn $ "netTask: " ++ err
        throwTo r e
        throwTo w e
    where
        spawn = do
            r <- forkIO $ netRead  s
            w <- forkIO $ netWrite s
            return (r,w)
        idle = do
            delayOneSec
            putStrLn "netTask: tick"

netRead :: MVar Scurry -> IO ()
netRead s = genericCatch "netRead" task
    where
        task = forever $ idle
        idle = do
            delayOneSec
            putStrLn "netRead: tick"

netWrite :: MVar Scurry -> IO ()
netWrite s = genericCatch "netWrite" task
    where
        task = forever $ idle
        idle = do
            delayOneSec
            putStrLn "netWrite: tick"

genericCatch :: String -> IO () -> IO ()
genericCatch id a = do
    catch a (gc id)
    where
        gc i e = do
            let err = show (e :: SomeException)
            putStrLn $ i ++ ": " ++ err
