{-# LANGUAGE CPP #-}

module Database.Tds.Transport (contextNew) where

import Data.Monoid((<>),mempty)
import Control.Applicative((<$>),(<*>))

import Network.Socket (Socket,close)
import Network.Socket.ByteString (recv,sendAll)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB

import Data.Binary (decode,encode)

import Data.Default.Class (def)
import qualified Network.TLS as TLS
import Network.TLS (ClientParams(..),Supported(..),Shared(..),ValidationCache(..),ValidationCacheResult(..))
import Network.TLS.Extra.Cipher (ciphersuite_strong)
import Data.X509.CertificateStore (CertificateStore(..))
import System.X509 (getSystemCertificateStore)

import Control.Concurrent(MVar(..),newMVar,readMVar,modifyMVar_)

import Database.Tds.Message.Header
#if !MIN_VERSION_tls(1,3,0)
import Crypto.Random(createEntropyPool,cprgCreate,SystemRNG(..))
#endif


-- | [\[MS-TDS\] 3.2.5.2 Sent TLS/SSL Negotiation Packet State](https://docs.microsoft.com/en-us/openspecs/windows_protocols/ms-tds/d62e225b-d865-4ccc-8f73-de1ef49e30d4)
contextNew :: Socket -> String -> IO TLS.Context
contextNew sock host = do
  certStore <- getSystemCertificateStore
  sock' <- newSecureSocket sock
#if MIN_VERSION_tls(1,3,0)
  TLS.contextNew (getBackend sock') (getTlsParams host certStore)
#else
  pool <- createEntropyPool
  TLS.contextNew (getBackend sock') (getTlsParams host certStore) (cprgCreate pool :: SystemRNG)
#endif



data SecureSocket = SecureSocket{ getSocket::Socket
                                , getSendBuff::MVar B.ByteString
                                , getSendStep::MVar Int
                                , getRecvBuff::MVar B.ByteString
                                }
                    
newSecureSocket sock = SecureSocket sock <$> newMVar mempty <*> newMVar 0 <*> newMVar mempty

getBackend sock' = TLS.Backend flush (close sock) sendAll' recvAll
      where
        sock = getSocket sock'
              
        flush = return()
          
        -- [MEMO] Put them into TDS packets at regular intervals
        -- [TODO] Consider a better implementation
        sendAll' bs = do
          step <- readMVar (getSendStep sock')
          case step of
            0 -> sendAll sock $ (header bs) <> bs --0x16
            1 -> appendBuff -- 0x16
            2 -> appendBuff -- 0x14
            3 -> do
              buff <- readMVar (getSendBuff sock')
              let bs' = buff <> bs
              sendAll sock $ (header bs') <> bs' -- 0x16
              modifyMVar_ (getSendBuff sock') (\_ -> return mempty)
            _ -> sendAll sock bs -- 0x17
          modifyMVar_ (getSendStep sock') (return . (+1))
            where
              appendBuff = modifyMVar_ (getSendBuff sock') (return . (<>bs))
              header bs = LB.toStrict $ encode $ Header 0x12 1 (B.length bs +8) 0 0 0
          
        -- [MEMO] This doesn't work
        -- [MEMO] Want to do this
        sendAll'' bs = do
          case B.head bs of
            0x17 -> sendAll sock bs
            _    -> sendAll sock $ (header bs) <> bs
            where
              header bs = LB.toStrict $ encode $ Header 0x12 1 (B.length bs +8) 0 0 0
          

        -- [MEMO] Remove TDS header
        -- [MEMO] Receive as much as possible from the source. and return only sink's requested size for each turn.
        -- [TODO] Consider a better implementation
        recvAll len = do
          buff <- readMVar (getRecvBuff sock')
          if B.null buff
            then recvDropBuff
            else dropBuff
                 
            where
              recvDropBuff = do
                header <- recv sock 8
                let (Header _ _ totalLen _ _ _) = decode $ LB.fromStrict header
                body <- recv sock $ totalLen -8
                let bs = B.take len body
                modifyMVar_ (getRecvBuff sock') (\_ -> return $ B.drop len body)
                return bs
                
              dropBuff = do
                buff <- readMVar (getRecvBuff sock')
                let bs = B.take len buff
                modifyMVar_ (getRecvBuff sock') (\_ -> return $ B.drop len buff)
                return bs


getTlsParams :: String -> CertificateStore -> ClientParams
getTlsParams host store =
  (TLS.defaultParamsClient host mempty) { clientSupported = def { supportedVersions = [TLS.TLS10]
                                                                , supportedCiphers = ciphersuite_strong
                                                                }
                                        , clientShared = def { sharedCAStore = store
                                                             , sharedValidationCache = validateCache
                                                             }
                                        }
  where
    validateCache = ValidationCache (\_ _ _ -> return ValidationCachePass) (\_ _ _ -> return ())

