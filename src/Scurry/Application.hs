module Scurry.Application (
    begin,
) where

import Control.Monad
import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.MVar

import System.Exit

import Scurry.Crypto
import Scurry.Scurry
import Scurry.Exception
import Scurry.UI
import Scurry.UI.Events
import Scurry.TAPTask
import Scurry.NetTask

data ScurryThreads = ScurryThreads {
    st_ui :: Maybe ThreadId
}

emptyThreads :: ScurryThreads
emptyThreads = ScurryThreads {
    st_ui = Nothing
}

{-
 - Our primary loop manages event information from the user
 - 
 - We accept event information on a channel and send responses
 - to the generating channel.
 -}

begin :: Scurry -> IO ()
begin s = do
    threads   <- newMVar emptyThreads
    state     <- newMVar s
    events    <- newEmptyMVar
    responses <- newEmptyMVar

    startUI threads state events responses

    let readC = takeMVar events
        writeC = putMVar responses
        die = stopThreads threads >> exitSuccess

    forever $ do
        (UIEvent txid code) <- readC
        case code of
            Shutdown -> writeC (UIResponse txid OK) >> shutdownWait >> die
            NoEvent  -> return ()

    where
        shutdownWait = do
            putStrLn "Scurry going down..."
            threadDelay $ 5 * 1000000

stopThreads :: (MVar ScurryThreads) -> IO ()
stopThreads t = do
    stopUI t

startUI :: MVar ScurryThreads
        -> MVar Scurry
        -> MVar UIEvent
        -> MVar UIResponse
        -> IO ()
startUI t state events responses =
    modifyMVar_ t $ \t' -> do
        uiT <- forkIO $ ui state events responses
        return $ t' { st_ui = Just uiT }

stopUI :: MVar ScurryThreads -> IO ()
stopUI threads = do
    modifyMVar_ threads $ \t -> do
        case st_ui t of
            (Just u) -> killThread u
            Nothing -> return ()
        return $ t { st_ui = Nothing }
    yield
        
