{-# OPTIONS_HADDOCK hide #-}
-- Stream Types:    https://msdn.microsoft.com/en-us/library/dd303435.aspx
-- Data Types:      https://msdn.microsoft.com/en-us/library/dd305325.aspx
-- Data Stream:     https://msdn.microsoft.com/en-us/library/dd340794.aspx
-- Client Messages: https://msdn.microsoft.com/en-us/library/dd341027.aspx


module Database.Tds.Message.Client ( Login7 (..)
                                   , defaultLogin7
                                   
                                   , SqlBatch (..)
                                   
                                   , RpcRequest (..)
                                   
                                   , RpcReqBatch (..)
                                   , ProcID (..)
                                   , ProcName (..)
                                   , OptionFlags (..)
                                   
                                   , RpcReqBatchParam (..)
                                   , ParamName (..)
                                   , StatusFlag (..)
                                   
                                   ) where

import Data.Monoid((<>))

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.Word (Word8(..),Word16(..),Word32(..),Word64(..))
import Data.Int (Int8(..),Int16(..),Int32(..),Int64(..))

import Data.Binary (Put(..),Get(..),Binary(..))
import qualified Data.Binary.Put as Put
import qualified Data.Binary.Get as Get

import Data.Bits ((.&.),(.|.),xor,shift)

import Control.Monad (foldM,foldM_)

import Database.Tds.Message.Prelogin
import Database.Tds.Message.DataStream
import Database.Tds.Primitives.Collation




data Login7 = Login7 { l7TdsVersion :: !Word32
                     , l7PacketSize :: !Word32
                     , l7ClientProgVer :: !Word32
                     , l7ConnectionID :: !Word32
                     , l7OptionFlags1 :: !Word8
                     , l7OptionFlags2 :: !Word8
                     , l7OptionFlags3 :: !Word8
                     , l7TypeFlags :: !Word8
                     , l7TimeZone :: !Word32
                     , l7Collation :: !Collation32
                     , l7CltIntName :: !T.Text
                     , l7Language :: !T.Text
                     , l7ClientPID :: !Word32
                     , l7ClientMacAddr :: !B.ByteString
                     , l7ClientHostName :: !T.Text
                     , l7AppName :: !T.Text
                     , l7ServerName :: !T.Text
                     , l7UserName :: !T.Text
                     , l7Password :: !T.Text
                     , l7Database :: !T.Text
                     }
            deriving (Show)

defaultLogin7 :: Login7
defaultLogin7 = Login7 { l7TdsVersion = 0x71000001
                         , l7PacketSize = 4096
                         , l7ClientProgVer = 0x0683f2f8
                         , l7ConnectionID = 0
                         , l7OptionFlags1 = 0x80 + 0x40 + 0x20
                         , l7OptionFlags2 = 0
                         , l7OptionFlags3 = 0
                         , l7TypeFlags = 0
                         , l7TimeZone = -120
--                         , l7Collation = 0x36040000
                         , l7Collation = 0x1104d000
                         , l7CltIntName = T.pack "DB-Library" -- "OLEDB", "ODBC"
                         , l7Language = mempty
--                         , l7Language = T.pack "us_english"
                         , l7ClientPID = 0
                         , l7ClientMacAddr = mempty
                         , l7ClientHostName = mempty
                         , l7AppName = mempty
                         , l7ServerName = mempty
                         , l7UserName = mempty
                         , l7Password = mempty
                         , l7Database = mempty
                         }


login7Bytes1 :: Login7 -> [B.ByteString]
login7Bytes1 x =
  let
    clientHostName  = T.encodeUtf16LE $ l7ClientHostName x
    userName        = T.encodeUtf16LE $ l7UserName x
    password        = cryptPassword $ T.encodeUtf16LE $ l7Password x
    appName         = T.encodeUtf16LE $ l7AppName x
    serverName      = T.encodeUtf16LE $ l7ServerName x
    unused          = mempty
    libraryName     = T.encodeUtf16LE $ l7CltIntName x
    language        = T.encodeUtf16LE $ l7Language x
    database        = T.encodeUtf16LE $ l7Database x

  in [ clientHostName
     , userName
     , password
     , appName
     , serverName
     , unused
     , libraryName
     , language
     , database
     ]
  where
    cryptPassword :: B.ByteString -> B.ByteString
    cryptPassword bs =
      let
        bs' = B.unpack bs
      in B.pack $ map (\x -> (shift x 4 .|. shift x (-4)) `xor` 0xa5) bs'
         

login7Bytes2 :: Login7 -> [B.ByteString]
login7Bytes2 x =
  let
    sspi            = mempty
    atachDBFile     = mempty
--    changePassword  = mempty -- TDS 7.2
  in [ sspi
     , atachDBFile
--     , changePassword -- TDS 7.2
     ]


login7HeaderLength :: Int
login7HeaderLength =
  let
    hLen = 4 -- payload length
         + 4 -- TDS Version
         + 4 -- packet size
         + 4 -- client program version
         + 4 -- client pid
         + 4 -- connection id
         + 1 -- flag1
         + 1 -- flag2
         + 1 -- SQL type
         + 1 -- flag3
         + 4 -- time zone
         + 4 -- collation
         + 4 -- idx client hostname 
         + 4 -- idx username
         + 4 -- idx password
         + 4 -- idx app name
         + 4 -- idx server name
         + 4 -- idx unused
         + 4 -- idx library name
         + 4 -- idx language
         + 4 -- idx database
         + 6 -- client mac addr
         + 4 -- idx SSPI
         + 4 -- idx AtachDBFile
--         + 4 -- idx ChangePassword -- TDS 7.2 
--         + 4 -- SSPI_long          -- TDS 7.2
  in hLen


login7Length :: Login7 -> Int
login7Length x =
  let
    bLen = sum $ map B.length $ login7Bytes1 x <> login7Bytes2 x
  in login7HeaderLength + bLen


-- https://msdn.microsoft.com/en-us/library/dd304019.aspx
putLogin7 :: Login7 -> Put
putLogin7 x = do
  Put.putWord32le $ fromIntegral plLen  -- payload length
  Put.putWord32le $ l7TdsVersion x
--	enum {
--		tds70Version = 0x70000000,
--		tds71Version = 0x71000001,
--		tds72Version = 0x72090002,
--		tds73Version = 0x730B0003,
--		tds74Version = 0x74000004,
--	};
  Put.putWord32le $ l7PacketSize x    -- packet size
  Put.putWord32le $ l7ClientProgVer x -- client program version
--  Put.putWord32be 0x00000007          -- client program version
  Put.putWord32le $ l7ClientPID x     -- client pid
  Put.putWord32le $ l7ConnectionID x  -- connect id
  Put.putWord8 $ l7OptionFlags1 x     -- flag1
  Put.putWord8 $ l7OptionFlags2 x     -- flag2
--  Put.putWord8 $ 0x02 + 0x01          -- flag2
  Put.putWord8 $ l7TypeFlags x        -- sql type
  Put.putWord8 $ l7OptionFlags3 x     -- flag3
  Put.putWord32le $ l7TimeZone x      -- tz
  Put.putWord32be $ l7Collation x     -- collation
--  Put.putWord32be $ 0x09040000        -- collation

  offs <- foldM putIndex plHLen bytes1  -- index 1st-half
  Put.putByteString $ l7ClientMacAddr x -- mac address
  foldM_ putIndex offs bytes2           -- index 2nd-half
  
--    Put.putWord32le 0 -- SSPI long  -- TDS 7.2

  mapM_ Put.putByteString $ bytes1 <> bytes2 -- datas

    where
      putIndex :: Int -> B.ByteString -> Put.PutM Int
      putIndex offs bs = do
        let
          len = B.length bs
        Put.putWord16le $ fromIntegral offs
        Put.putWord16le $ fromIntegral $ len `div` 2
        return $ offs+len

      plLen  = login7Length x
      plHLen = login7HeaderLength
      bytes1 = login7Bytes1 x
      bytes2 = login7Bytes2 x
          

getLogin7 :: Get Login7
getLogin7 = undefined -- [TODO] implement get function


instance Binary Login7 where
  put = putLogin7
  get = getLogin7
  




newtype SqlBatch  = SqlBatch T.Text
                  deriving (Show)


putSqlBatch :: SqlBatch -> Put
putSqlBatch (SqlBatch sql) = Put.putByteString $ T.encodeUtf16LE sql

getSqlBatch :: Get SqlBatch
getSqlBatch = undefined -- [TODO] implement get function


instance Binary SqlBatch where
  put = putSqlBatch
  get = getSqlBatch



type ParamName = T.Text
type StatusFlag = Word8

data RpcReqBatchParam = RpcReqBatchParam !ParamName !StatusFlag !TypeInfo !RawBytes
                      deriving (Show)


type ProcID = Word16
type ProcName = T.Text
type OptionFlags = Word16

data RpcReqBatch = RpcReqBatchProcId !ProcID !OptionFlags ![RpcReqBatchParam]
                 | RpcReqBatchProcName !ProcName !OptionFlags ![RpcReqBatchParam]
                 deriving (Show)


newtype RpcRequest = RpcRequest [RpcReqBatch]
                   deriving (Show)
                    





putRpcReqBatch :: RpcReqBatch -> Put
putRpcReqBatch (RpcReqBatchProcId pid flgs pds) = do
  Put.putWord16le 0xffff
  Put.putWord16le pid
  Put.putWord16le flgs
  mapM_ putRpcReqBatchParam pds
  
putRpcReqBatch (RpcReqBatchProcName pn flgs pds) = do
  Put.putWord16le $ fromIntegral $ T.length pn -- [MEMO] text length
  Put.putByteString $ T.encodeUtf16LE pn
  Put.putWord16le flgs
  mapM_ putRpcReqBatchParam pds
  

putRpcReqBatchParam :: RpcReqBatchParam -> Put
putRpcReqBatchParam (RpcReqBatchParam pn sf ti val) = do
  Put.putWord8 $ fromIntegral $ T.length pn -- [MEMO] text length
  Put.putByteString $ T.encodeUtf16LE pn
  Put.putWord8 sf
  put ti
  putRawBytes ti val
  return ()


putRpcRequest :: RpcRequest -> Put
putRpcRequest (RpcRequest bts) = mapM_ (\bt -> putRpcReqBatch bt >> Put.putWord8 0x80) bts -- [MEMO] split by 0x80

getRpcRequest :: Get RpcRequest
getRpcRequest = undefined -- [TODO] implement get function


instance Binary RpcRequest where
  put = putRpcRequest
  get = getRpcRequest



