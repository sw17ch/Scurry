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
    -- readMVar cleanup
    threadDelay (5 * 1000000)

    putStrLn "Tear down UI..."
    throwTo ui_t  TearDown

    putStrLn "Tear down TAPTask..."
    throwTo tap_t TearDown

    putStrLn "Tear down NetTask..."
    throwTo net_t TearDown

    -- Murderous rampage time...

    putStrLn "Killing UI..."
    killThread ui_t

    putStrLn "Killing TAPTask..."
    killThread tap_t

    putStrLn "Killing NetTask..."
    killThread net_t

    -- Done!
    putStrLn "Done. Thanks for using Scurry."

