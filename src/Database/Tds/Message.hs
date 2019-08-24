module Database.Tds.Message ( -- * Client Message
                              ClientMessage (..)
                            , getClientMessage
                            , putClientMessage
                              
                            -- ** Login
                            , Login7
                            , tdsVersion
                            , defaultLogin7
                            , l7PacketSize
                            , l7ClientProgVer
                            , l7ConnectionID
                            , l7OptionFlags1
                            , l7OptionFlags2
                            , l7OptionFlags3
                            , l7TypeFlags
                            , l7TimeZone
                            , l7Collation
                            , l7CltIntName
                            , l7Language
                            , l7ClientPID
                            , l7ClientMacAddr
                            , l7ClientHostName
                            , l7AppName
                            , l7ServerName
                            , l7UserName
                            , l7Password
                            , l7Database
                            
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
                            , getServerMessage
                            , putServerMessage
                            
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
                            , Money (..)
                            
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

import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Builder as BB

import Data.Word (Word8(..),Word16(..),Word32(..),Word64(..))
import Data.Int (Int8(..),Int16(..),Int32(..),Int64(..))

import Data.Binary (Put(..),Get(..),Binary(..),decode,encode)
import qualified Data.Binary.Put as Put
import qualified Data.Binary.Get as Get

import Control.Monad.Writer (WriterT(..),runWriterT,tell)
import Control.Monad.Trans (lift)

import Database.Tds.Primitives.Null
import Database.Tds.Primitives.Decimal
import Database.Tds.Primitives.Money
import Database.Tds.Primitives.Collation

import Database.Tds.Message.Header
import Database.Tds.Message.DataStream
import Database.Tds.Message.Prelogin
import Database.Tds.Message.Client
import Database.Tds.Message.Server





putMessage :: Word32 -> Word8 -> LB.ByteString -> Put
putMessage ps pt bs = mapM_ f $ split (ps -headerLength) bs
  where
    f :: (Bool,LB.ByteString) -> Put
    f (isLast,bs) = do
      let
        len = (fromIntegral $ LB.length bs) + headerLength
        flg = if isLast then 0x01 else 0x00 -- last flag
      put $ Header pt flg len 0 0 0
      Put.putLazyByteString bs

  
    split :: Word32 -> LB.ByteString -> [(Bool,LB.ByteString)]
    split len lbs =
      let
        (lbs',rem) = LB.splitAt (fromIntegral len) lbs
      in if LB.null rem
         then [(True,lbs')]
         else (False,lbs'): split len rem


getMessage :: Get (Word8,LB.ByteString)
getMessage = (\(pt,bs) -> (pt,BB.toLazyByteString bs)) <$> runWriterT f
  where
    f :: WriterT BB.Builder Get Word8
    f = do
      (Header pt flg len _ _ _) <- lift get
      tell =<< BB.byteString <$> (lift $ Get.getByteString (fromIntegral $ len -8))
      if flg == 0x01
        then return pt
        else f




-- | [\[MS-TDS\] 2.2.1 Client Messages](https://docs.microsoft.com/en-us/openspecs/windows_protocols/ms-tds/7ea9ee1a-b461-41f2-9004-141c0e712935)
data ClientMessage = CMPrelogin !Prelogin
                   | CMLogin7 !Login7
                   | CMSqlBatch !SqlBatch
                   | CMRpcRequest !RpcRequest
                   deriving (Show)

putClientMessage :: Word32 -> ClientMessage -> Put
putClientMessage ps x =
  let (pt,bs) = case x of
        CMPrelogin   pr -> (0x12,encode pr)
        CMLogin7     l7 -> (0x10,encode l7)
        CMSqlBatch   b  -> (0x01,encode b)
        CMRpcRequest r  -> (0x03,encode r)
  in putMessage ps pt bs

getClientMessage :: Get ClientMessage
getClientMessage = do
  (pt,bs) <- getMessage
  case pt of
    0x12 -> return $ CMPrelogin   $ decode bs
    0x10 -> return $ CMLogin7     $ decode bs
    0x01 -> return $ CMSqlBatch   $ decode bs
    0x03 -> return $ CMRpcRequest $ decode bs
    _ -> fail "getClientMessage: invalid packet type"




-- | [\[MS-TDS\] 2.2.2 Server Messages](https://docs.microsoft.com/en-us/openspecs/windows_protocols/ms-tds/342f4cbb-2b4b-489c-8b63-f99b12021a94)
class Binary a => ServerMessage a
instance ServerMessage Prelogin
instance ServerMessage TokenStreams

putServerMessage :: ServerMessage a => Word32 -> a -> Put
putServerMessage ps x =
  putMessage ps 0x04 $ encode x

getServerMessage :: ServerMessage a => Get a
getServerMessage = do
  (pt,bs) <- getMessage
  case pt of
    0x04 -> return $ decode bs
    _ -> fail "getServerMessageInstance: invalid packet type"


