module Scurry.TAPSrc (
    readTAPTask,
) where

import Control.Monad
import Control.Concurrent.MVar

import Scurry.FFI
import Scurry.Scurry
import Scurry.Data.Packet

readTAPTask :: MVar Scurry -> MVar () -> IO ()
readTAPTask m c = do
    withTAP 1200 $ \d -> do
        setIP d 0x0100000A   -- 10.0.0.1
        setMask d 0x00FFFFFF -- 255.255.255.0
        getMAC d >>= print
        forever $ readTAP d >>= (print . parsePkt)
        return ()
    putMVar c ()
