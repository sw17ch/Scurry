module Scurry.TAPSrc (
    readTAPTask,
) where

import Control.Concurrent.MVar

import Scurry.FFI

readTAPTask m c = do
    test
    putMVar c ()
