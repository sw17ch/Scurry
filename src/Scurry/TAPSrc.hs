module Scurry.TAPSrc (
    readTAPTask,
) where

import Control.Concurrent.MVar

import Scurry.FFI
import Scurry.Scurry

readTAPTask :: MVar Scurry -> MVar () -> IO ()
readTAPTask m c = do
    test
    putMVar c ()
