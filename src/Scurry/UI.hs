module Scurry.UI where

import Scurry.Scurry
import Scurry.Config

import Hack
import qualified Hack.Handler.Happstack as H

import Control.Concurrent.MVar
import qualified Data.ByteString.Lazy.Char8 as BS

hackConf :: H.ServerConf
hackConf = H.ServerConf { H.port = 24999, H.serverName = "localhost" }

ui :: (MVar Scurry) -> IO ()
ui s = H.runWithConfig hackConf $ app s

app :: (MVar Scurry) -> Application
app s = \env -> do
    r <- uiIndex >>= BS.readFile
    return $ Response 200 [ ("Content-Type", "text/html") ] r
