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

    let readC = atomically $ readTChan events
        writeC = atomically . (writeTChan responses)
        die = do killThread uiT
                 yield
                 exitSuccess

    forever $ do
        (UIEvent txid code) <- atomically $ readTChan events
        case code of
            Shutdown -> writeC (UIResponse txid OK) >> shutdownWait >> die
            NoEvent  -> return ()

    where
        shutdownWait = do
            putStrLn "Scurry going down..."
            threadDelay $ 5 * 1000000
