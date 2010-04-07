module Scurry.Application (
    begin,
) where

import Control.Monad
import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan

import System.Exit

import Scurry.Crypto
import Scurry.Scurry
import Scurry.Exception
import Scurry.UI
import Scurry.UI.Events
import Scurry.TAPTask
import Scurry.NetTask

{-
 - Our primary loop manages event information from the user
 - 
 - We accept event information on a channel and send responses
 - to the generating channel.
 -}

begin :: Scurry -> IO ()
begin s = do
    state     <- newMVar s
    events    <- newTChanIO
    responses <- newTChanIO

    uiT <- forkIO $ ui state events responses

    let die = do killThread uiT
                 yield
                 exitSuccess

    forever $ do
        (UIEvent ident code) <- atomically $ readTChan events
        case code of
            Shutdown -> die

{-
begin :: Scurry -> IO ()
begin s = do
    scurry  <- newMVar s    -- Global state variable
    cleanup <- newEmptyMVar -- Global signal to cleanup

    ui_t  <- forkIO $ ui      scurry cleanup
    tap_t <- forkIO $ tapTask scurry cleanup
    net_t <- forkIO $ netTask scurry cleanup

    -- When we pass this, some one triggered a cleanup
    readMVar cleanup

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
-}
