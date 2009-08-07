module Main where

import Scurry.Scurry
import Scurry.Data.Network
import Scurry.UI
import Scurry.Exception
import Scurry.TAPSrc
import Scurry.Crypto

import Control.Concurrent
import Control.Concurrent.MVar

import System.Environment
import System.Exit
import System.Console.GetOpt

data Opt = OVPNAddr  VPNAddr
         | OVPNMask  IPV4Addr
         | OBindAddr IPV4Addr
         | OBindPort IPPort
         | OPeerName String

options :: [OptDescr Opt]
options =
    [ Option "a" ["vpn-addr"]  (ReqArg rVPNAddr "VPN_ADDRESS")
        "IPV4 address to use for VPN"
    , Option "m" ["vpn-mask"]  (ReqArg rVPNMask "VPN_NETMASK")
        "IPV4 address to use for VPN mask"
    , Option "b" ["bind-addr"] (ReqArg rBindAddr "BIND_ADDRESS")
        "address to bind for tunnels"
    , Option "p" ["bind-port"] (ReqArg rBindPort "BIND_PORT")
        "port to bind for tunnels"
    , Option "n" ["peer-name"] (ReqArg rPeerName "PEER_NAME")
        "name of this peer" ]
    where
        rVPNAddr  = OVPNAddr  . read
        rVPNMask  = OVPNMask  . read
        rBindAddr = OBindAddr . read
        rBindPort = OBindPort . read
        rPeerName = OPeerName

parseOpts :: [String] -> (Either String Scurry)
parseOpts args = case args of
                      ("-h":_)     -> Left  usage
                      ("--help":_) -> Left  usage
                      _            -> Right normal
    where
        usage  = usageInfo "How to use scurry." options
        normal = foldl repl defaultScurry opts

        (opts,_,_) = getOpt RequireOrder options args
        repl s o = case o of
                        OVPNAddr  a -> s { vpnAddr  = (Just a) }
                        OVPNMask  m -> s { vpnMask  = (Just m) }
                        OBindAddr a -> s { bindAddr = a }
                        OBindPort p -> s { bindPort = p }
                        OPeerName n -> s { peerName = n }

begin :: (MVar Scurry) -> IO ()
begin m = do
    -- Cleanup gets set when the connected UI agent
    -- signals that things should wrap up and kill
    -- the process.
    cleanup <- newEmptyMVar

    ui_t  <- forkIO $ ui          m cleanup
    tap_t <- forkIO $ readTAPTask m cleanup

    takeMVar cleanup

    -- Inform the UI thread that we're cleaning up
    throwTo ui_t  TearDown
    throwTo tap_t TearDown

    -- Wait 5 seconds for everything to clean up
    threadDelay (10 * 1000000)

    -- Done!

main :: IO ()
main = withSocketsDo $ do
    putStrLn "Scurry 0.1.0"

    m <- newEmptyMVar

    getRSAKey

    args <- getArgs
    case parseOpts args of
        (Left  u) -> putStrLn u  >> exitFailure
        (Right s) -> putMVar m s >> begin m
