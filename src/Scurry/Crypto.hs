module Scurry.Crypto (
    getRSAKey
) where

import System.Directory
import OpenSSL.RSA
import OpenSSL.PEM
import OpenSSL.EVP.PKey

import Scurry.Config

getRSAKey :: IO RSAKeyPair
getRSAKey = do
    dd <- userDataDir
    
    let pub = dd ++ "/pubKey"
        prv = dd ++ "/prvKey"

    pube <- doesFileExist pub
    prve <- doesFileExist prv

    case pube && prve of
         True  -> do
                (Just k) <- openRSAKey pub prv
                return k

         False -> do
                k <- generateRSAKey' 2048 65537
                writeRSAKey k pub prv
                return k

openRSAKey :: FilePath -> FilePath -> IO (Maybe RSAKeyPair)
openRSAKey pub prv = do
    kf <- readFile prv
    k  <- readPrivateKey kf PwNone
    return . toKeyPair $ k

writeRSAKey :: RSAKeyPair -> FilePath -> FilePath -> IO ()
writeRSAKey key pub prv = do
    writePublicKey key >>= writeFile pub
    writePKCS8PrivateKey key Nothing >>= writeFile prv
    
