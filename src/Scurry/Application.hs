module Scurry.Application (
    begin,
) where

import Control.Monad
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
    st_ui  :: Maybe ThreadId,
    st_tap :: Maybe ThreadId
}

emptyThreads :: ScurryThreads
emptyThreads = ScurryThreads {
    st_ui  = Nothing,
    st_tap = Nothing
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
            Shutdown -> do
                writeC (UIResponse txid OK)
                shutdownWait
                die
            Start    -> do
                startTAP threads state
                writeC (UIResponse txid OK)
            NoEvent  -> return ()

    where
        shutdownWait = do
            putStrLn "Scurry going down..."
            mapM_ (\n ->
                print n >> threadDelay 1000000) (reverse [1..5] :: [Int])

stopThreads :: (MVar ScurryThreads) -> IO ()
stopThreads t = do
    stopUI t
    stopTAP t

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
stopUI ts = do
    modifyMVar_ ts $ \t -> do
        case st_ui t of
            (Just u) -> killThread u
            Nothing -> return ()
        return $ t { st_ui = Nothing }
    yield
        
startTAP :: MVar ScurryThreads
         -> MVar Scurry
         -> IO ()
startTAP ts state = do
    modifyMVar_ ts $ \t' -> do
        tapT <- forkIO $ tapTask state
        return $ t' { st_tap = Just tapT }

stopTAP :: MVar ScurryThreads -> IO ()
stopTAP ts = do
    modifyMVar_ ts $ \t -> do
        case st_tap t of
            (Just p) -> killThread p
            Nothing  -> return ()
        return $ t { st_tap = Nothing }
    yield
