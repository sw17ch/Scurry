module Scurry.TAPTask (
    tapTask,
) where

import Prelude hiding (catch)

import Control.Monad
import Control.Concurrent.MVar
import Control.Exception

import Scurry.Scurry
import Scurry.Data.Packet

import Network.TUNTAP

tapDbg :: String -> IO ()
tapDbg = putStrLn

tapTask :: MVar Scurry -> MVar () -> IO ()
tapTask m c = catch go hdlE
    where
        go = do withTAP 1200 $ \d -> do
                    setIP d 0x0100000A   -- 10.0.0.1
                    setMask d 0x00FFFFFF -- 255.255.255.0
                    getMAC d >>= print
                    forever $ readTAP d >>= (print . parsePkt)
                    return ()
                putMVar c ()

        hdlE e = do
            let err = show (e :: SomeException)
            tapDbg $ "tapTask: " ++ err
