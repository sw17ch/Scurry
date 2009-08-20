module Scurry.Application (
    begin,
) where

import Control.Concurrent
import Control.Concurrent.MVar

import Scurry.Crypto
import Scurry.Scurry
import Scurry.Exception
import Scurry.UI
import Scurry.TAPTask
import Scurry.NetTask

begin :: Scurry -> IO ()
begin s = do
    scurry  <- newMVar s    -- Global state variable
    cleanup <- newEmptyMVar -- Global signal to cleanup

    ui_t  <- forkIO $ ui      scurry cleanup
    tap_t <- forkIO $ tapTask scurry cleanup
    net_t <- forkIO $ netTask scurry cleanup

    -- When we pass this, some one triggered a cleanup
    readMVar cleanup

    throwTo ui_t  TearDown
    throwTo tap_t TearDown
    throwTo net_t TearDown

    -- Wait 10 seconds for everything to clean up
    threadDelay (10 * 1000000)

    -- Done!
