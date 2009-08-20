module Scurry.Crypto (
    getRSAKey,
    encryptBlk,
    ScurryPubKey,
    EncKey, -- |Encrypted Key
    EncBlkInfo,
) where

import Control.Monad
import Data.Maybe
import System.Directory

import OpenSSL.RSA
import OpenSSL.PEM
import OpenSSL.EVP.PKey
import OpenSSL.EVP.Cipher
import OpenSSL.EVP.Seal

import Scurry.Config

import qualified Data.ByteString.Char8 as C

newtype EncKey = EncKey C.ByteString
newtype InitVector = IV C.ByteString

newtype ScurryPubKey = ScurryPubKey { unScurryPubKey :: SomePublicKey }

instance Show ScurryPubKey where
    show _ = "ScurryPubKey"

data EncBlkInfo = EncBlkInfo {
    message :: C.ByteString,
    keys :: [EncKey],
    ivec :: InitVector
}

encryptBlk :: C.ByteString -> [ScurryPubKey] -> IO EncBlkInfo
encryptBlk msg pks = do
    c <- liftM fromJust $ getCipherByName "AES256-SHA"
    (msg', eks, iv) <- sealBS c (map unScurryPubKey pks) msg
    return $ EncBlkInfo { message = msg',
                          keys = map (EncKey . C.pack) eks,
                          ivec = IV . C.pack $ iv }

getRSAKey :: IO RSAKeyPair
getRSAKey = do
    dd <- userDataDir
    
    let pub = dd ++ "/pubKey"
        prv = dd ++ "/prvKey"

    pube <- doesFileExist pub
    prve <- doesFileExist prv

    case pube && prve of
         True  -> do
                k <- openRSAKey pub prv
                case k of
                    Nothing   -> error $ "Not a private RSA key: " ++ prv
                    (Just k') -> return k'

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
    
