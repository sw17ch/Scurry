module Scurry.Config (
    appName,
    userDataDir,
    uiIndex,
    uiJQuery,
    uiStyle,
    uiScript,
) where

import System.Directory
import Paths_Scurry

appName :: String
appName = "scurry"

userDataDir :: IO FilePath
userDataDir = do
    d <- getAppUserDataDirectory appName
    createDirectoryIfMissing True d
    return d

uiIndex :: IO FilePath
uiIndex = getDataFileName "ui/index.html"

uiJQuery :: IO FilePath
uiJQuery = getDataFileName "ui/jquery-1.3.1.js"

uiStyle :: IO FilePath
uiStyle = getDataFileName "ui/style.css"

uiScript :: IO FilePath
uiScript = getDataFileName "ui/script.js"
