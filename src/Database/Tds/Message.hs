module Database.Tds.Message ( -- * Client Message
                              ClientMessage (..)
                              
                            -- ** Login
                            , Login7
                            , defaultLogin7
                            , l7ConnectionID
                            , l7OptionFlags1
                            , l7OptionFlags2
                            , l7OptionFlags3
                            , l7TypeFlags
                            , l7TimeZone
                            , l7Collation
                            , l7Language
                            , l7ClientPID
                            , l7ClientMacAddr
                            , l7ClientHostName
                            , l7AppName
                            , l7ServerName
                            , l7UserName
                            , l7Password
                            , l7Database
                            , tdsVersion
                            
                            -- ** SQL Batch
                            , SqlBatch (..)
                            
                            -- ** RPC Request
                            , RpcRequest (..)
                            
                            , RpcReqBatch (..)
                            , ProcID (..)
                            , ProcName (..)
                            , OptionFlags (..)

                            , RpcReqBatchParam (..)
                            , ParamName (..)
                            , StatusFlag (..)
                            
                            -- * Server Message
                            , ServerMessage (..)
                            , ServerMessageInstance (..)
                            
                            , TokenStreams (..)
                            , TokenStream (..)
                            
                            -- ** AltMetaData
                            , AltMetaData (..)
                            
                            -- ** AltRowData
                            , AltRowData (..)

                            -- ** ColProperty
                            , ColProperty (..)
                            , CPColNum (..)
                            , CPTableNum (..)
                            , CPStatus (..)
                            , CPColName (..)
                            
                            -- ** ColMetaData
                            , ColMetaData (..)
                            , MetaColumnData (..)
                            , MCDUserType (..)
                            , MCDFlags (..)
                            , MCDTableName (..)
                            , MCDColName (..)

                            -- ** Done, DoneInProc, DoneProc
                            , Done (..)
                            , DoneStatus (..)
                            , DoneCurCmd (..)
                            , DoneRowCount (..)
                            
                            -- ** EnvChange
                            , ECType (..)
                            , ECNewValue (..)
                            , ECOldValue (..)

                            -- ** Error, Info
                            , Info (..)
                            , InfoNumber (..)
                            , InfoState (..)
                            , InfoClass (..)
                            , InfoMsgText (..)
                            , InfoServerName (..)
                            , InfoProcName (..)
                            , InfoLineNumber (..)
                            
                            -- ** LoginAck
                            , LAInterface (..)
                            , LATdsVersion (..)
                            , LAProgName (..)
                            , LAProgVersion (..)
                            
                            -- ** Offset
                            , OffsetIdentifier (..)
                            , OffsetLength (..)
                            , Offset (..)
                            
                            -- ** ReturnValue
                            , ReturnValue (..)
                            , RVParamOrdinal (..)
                            , RVParamName (..)
                            , RVStatus (..)
                            , RVUserType (..)
                            , RVFlags (..)
                            
                            -- ** Row
                            , RowColumnData (..)
                            , TextPointer (..)
                            , TimeStamp (..)

                            -- * Primitives
                            , TypeInfo (..)
                            , RawBytes (..)
                            , Data (..)

                            , Null (..)
                            
                            , Precision (..)
                            , Scale (..)
                            , Decimal (..)
                            , Money (..)
                            , decimal0,decimal1,decimal2,decimal3,decimal4
                            , decimal5,decimal6,decimal7,decimal8,decimal9
                            , decimal10,decimal11,decimal12,decimal13,decimal14
                            , decimal15,decimal16,decimal17,decimal18,decimal19
                            , decimal20,decimal21,decimal22,decimal23,decimal24
                            , decimal25,decimal26,decimal27,decimal28,decimal29
                            , decimal30,decimal31,decimal32,decimal33,decimal34
                            , decimal35,decimal36,decimal37,decimal38
                            
                            , Collation (..)
                            , Collation32 (..)
                            , SortId (..)

                              
                            -- * Prelogin
                            , Prelogin (..)
                            , PreloginOption (..)
                            , MajorVer (..)
                            , MinorVer (..)
                            , BuildVer (..)
                            , SubBuildVer (..)
                            , Threadid (..)
                            , Connid (..)
                            , Activity (..)
                            , Sequence (..)
                            , Nonce (..)
                            
                            ) where

import Control.Applicative((<$>))

import Data.Word (Word8(..),Word16(..),Word32(..),Word64(..))
import Data.Int (Int8(..),Int16(..),Int32(..),Int64(..))

import Data.Binary (Put(..),Get(..),Binary(..),decode,encode)
import qualified Data.Binary.Put as Put
import qualified Data.Binary.Get as Get

import Database.Tds.Primitives.Null
import Database.Tds.Primitives.Decimal
import Database.Tds.Primitives.Money
import Database.Tds.Primitives.Collation

import Database.Tds.Message.Header
import Database.Tds.Message.DataStream
import Database.Tds.Message.Prelogin
import Database.Tds.Message.Client
import Database.Tds.Message.Server






-- | [\[MS-TDS\] 2.2.1 Client Messages](https://docs.microsoft.com/en-us/openspecs/windows_protocols/ms-tds/7ea9ee1a-b461-41f2-9004-141c0e712935)
data ClientMessage = CMPrelogin !Prelogin
                   | CMLogin7 !Login7
                   | CMSqlBatch !SqlBatch
                   | CMRpcRequest !RpcRequest
                   deriving (Show)

putClientMessage :: ClientMessage -> Put
putClientMessage x =
  let (pt,bs) = case x of
        CMPrelogin   pr -> (0x12,encode pr)
        CMLogin7     l7 -> (0x10,encode l7)
        CMSqlBatch   b  -> (0x01,encode b)
        CMRpcRequest r  -> (0x03,encode r)
  in putMessage pt bs

getClientMessage :: Get ClientMessage
getClientMessage = do
  (pt,bs) <- getMessage
  case pt of
    0x12 -> return $ CMPrelogin   $ decode bs
    0x10 -> return $ CMLogin7     $ decode bs
    0x01 -> return $ CMSqlBatch   $ decode bs
    0x03 -> return $ CMRpcRequest $ decode bs
    _ -> fail "getClientMessage: invalid packet type"

instance Binary ClientMessage where
  put = putClientMessage
  get = getClientMessage



class Binary a => ServerMessageInstance a
instance ServerMessageInstance Prelogin
instance ServerMessageInstance TokenStreams

putServerMessageInstance :: ServerMessageInstance a => a -> Put
putServerMessageInstance x =
  putMessage 0x04 $ encode x

getServerMessageInstance :: ServerMessageInstance a => Get a
getServerMessageInstance = do
  (pt,bs) <- getMessage
  case pt of
    0x04 -> return $ decode bs
    _ -> fail "getServerMessageInstance: invalid packet type"


-- | [\[MS-TDS\] 2.2.2 Server Messages](https://docs.microsoft.com/en-us/openspecs/windows_protocols/ms-tds/342f4cbb-2b4b-489c-8b63-f99b12021a94)
newtype ServerMessage a = ServerMessage a
                        deriving (Show)

instance (ServerMessageInstance a) => Binary (ServerMessage a) where
  put (ServerMessage x) = putServerMessageInstance x
  get = ServerMessage <$> getServerMessageInstance


