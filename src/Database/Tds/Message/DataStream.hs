{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE FlexibleInstances #-}
-- Stream Types:    https://msdn.microsoft.com/en-us/library/dd303435.aspx
-- Data Types:      https://msdn.microsoft.com/en-us/library/dd305325.aspx
-- Data Stream:     https://msdn.microsoft.com/en-us/library/dd340794.aspx


module Database.Tds.Message.DataStream ( TypeInfo (..)
                                       , RawBytes (..)
                                       , getRawBytes
                                       , putRawBytes
                                       , Data (..)
                                       ) where

import Data.Monoid((<>))
import Control.Applicative((<$>),(<*>))

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT

import Data.Word (Word8(..),Word16(..),Word32(..),Word64(..))
import Data.Int (Int8(..),Int16(..),Int32(..),Int64(..))

import Data.Binary (Put(..),Get(..),Binary(..))
import qualified Data.Binary.Put as Put
import qualified Data.Binary.Get as Get

import Data.Time (UTCTime(..))
import Data.UUID.Types (UUID)
import qualified Data.UUID.Types as UUID

import Database.Tds.Primitives.Null
import Database.Tds.Primitives.Money
import Database.Tds.Primitives.DateTime
import Database.Tds.Primitives.Float
import Database.Tds.Primitives.Decimal
import Database.Tds.Primitives.Collation





-- | [\[MS-TDS\] 2.2.5.4 Data Type Definitions](https://docs.microsoft.com/en-us/openspecs/windows_protocols/ms-tds/ffb02215-af07-4b50-8545-1fd522106c68)
data TypeInfo = TINull       -- 0x1f
              | TIBit        -- 0x32
              | TIInt1       -- 0x30
              | TIInt2       -- 0x34
              | TIInt4       -- 0x38
              | TIInt8       -- 0x7f
              | TIMoney4     -- 0x7a
              | TIMoney8     -- 0x3c
              | TIDateTime4  -- 0x3a
              | TIDateTime8  -- 0x3d
              | TIFlt4       -- 0x3b
              | TIFlt8       -- 0x3e
                
              | TIBitN        -- 0x68
              | TIIntN1       -- 0x26
              | TIIntN2       -- 0x26
              | TIIntN4       -- 0x26
              | TIIntN8       -- 0x26
              | TIMoneyN4     -- 0x6e
              | TIMoneyN8     -- 0x6e
              | TIDateTimeN4  -- 0x6f
              | TIDateTimeN8  -- 0x6f
              | TIFltN4       -- 0x6d
              | TIFltN8       -- 0x6d
                
              | TIGUID                     -- 0x24
              | TIDecimalN !Precision !Scale -- 0x6a, 0x37(legacy)
              | TINumericN !Precision !Scale -- 0x6c, 0x3f(legacy)
                
              | TIChar !Word8                  -- 0x2f(legacy) -- [TODO] test
              | TIVarChar !Word8               -- 0x27(legacy) -- [TODO] test
              | TIBigChar !Word16 !Collation    -- 0xaf
              | TIBigVarChar !Word16 !Collation -- 0xa7
              | TIText !Word32 !Collation       -- 0x23
                
              | TINChar !Word16 !Collation    -- 0xef
              | TINVarChar !Word16 !Collation -- 0xe7
              | TINText !Word32 !Collation    -- 0x63
                
              | TIBinary !Word8        -- 0x2d(legacy) -- [TODO] test
              | TIVarBinary !Word8     -- 0x25(legacy) -- [TODO] test
              | TIBigBinary !Word16    -- 0xad
              | TIBigVarBinary !Word16 -- 0xa5
              | TIImage !Word32        -- 0x22
              deriving (Show)


-- https://msdn.microsoft.com/en-us/library/dd358284.aspx
-- https://msdn.microsoft.com/en-us/library/dd305325.aspx
getTypeInfo :: Get TypeInfo
getTypeInfo = f =<< Get.getWord8
  where
    f :: Word8 -> Get TypeInfo
    f 0x1f = return TINull
    f 0x32 = return TIBit
    f 0x30 = return TIInt1
    f 0x34 = return TIInt2
    f 0x38 = return TIInt4
    f 0x7f = return TIInt8
    f 0x7a = return TIMoney4
    f 0x3c = return TIMoney8
    f 0x3a = return TIDateTime4
    f 0x3d = return TIDateTime8
    f 0x3b = return TIFlt4
    f 0x3e = return TIFlt8

    f 0x68 = do
      _ <- Get.getWord8
      return TIBitN

    f 0x26 = do
      len <- Get.getWord8
      case len of
        1 -> return TIIntN1
        2 -> return TIIntN2
        4 -> return TIIntN4
        8 -> return TIIntN8
        _ -> fail "getTypeInfo: invalid data length"

    f 0x6e = do
      len <- Get.getWord8
      case len of
        4 -> return TIMoneyN4
        8 -> return TIMoneyN8
        _ -> fail "getTypeInfo: invalid data length"

    f 0x6f = do
      len <- Get.getWord8
      case len of
        4 -> return TIDateTimeN4
        8 -> return TIDateTimeN8
        _ -> fail "getTypeInfo: invalid data length"
  
    f 0x6d = do
      len <- Get.getWord8
      case len of
        4 -> return TIFltN4
        8 -> return TIFltN8
        _ -> fail "getTypeInfo: invalid data length"
  
    f 0x24 = do
      len <- Get.getWord8 -- 0x10 (16byte)
      case len of
        16 -> return TIGUID
        _ -> fail "getTypeInfo: invalid data length"
  
    f 0x37 = f 0x6a
    f 0x6a = do
      _ <- Get.getWord8
      TIDecimalN <$> Get.getWord8 -- precision
                 <*> Get.getWord8 -- scale

    f 0x3f = f 0x6c
    f 0x6c = do
      _ <- Get.getWord8
      TINumericN <$> Get.getWord8 -- precision
                 <*> Get.getWord8 -- scale

    -- [TODO] test
    -- [MEMO] no collation
    f 0x2f = TIChar <$> Get.getWord8

    -- [TODO] test
    -- [MEMO] no collation
    f 0x27 = TIVarChar <$> Get.getWord8

    f 0xaf = TIBigChar <$> Get.getWord16le
                       <*> getCollation -- collation

    f 0xa7 = TIBigVarChar <$> Get.getWord16le
                          <*> getCollation -- collation

    f 0x23 = TIText <$> Get.getWord32le
                    <*> getCollation -- collation

    f 0xef = TINChar <$> Get.getWord16le
                     <*> getCollation -- collation

    f 0xe7 = TINVarChar <$> Get.getWord16le
                        <*> getCollation -- collation

    f 0x63 = TINText <$> Get.getWord32le
                     <*> getCollation -- collation

    -- [TODO] test
    f 0x2d = TIBinary <$> Get.getWord8

    -- [TODO] test
    f 0x25 = TIVarBinary <$> Get.getWord8

    f 0xad = TIBigBinary <$> Get.getWord16le

    f 0xa5 = TIBigVarBinary <$> Get.getWord16le

    f 0x22 = TIImage <$> Get.getWord32le



-- https://msdn.microsoft.com/en-us/library/dd358284.aspx
-- https://msdn.microsoft.com/en-us/library/dd305325.aspx
putTypeInfo :: TypeInfo -> Put
putTypeInfo (TINull     ) = Put.putWord8 0x1f -- [TODO] test
putTypeInfo (TIBit      ) = Put.putWord8 0x32
putTypeInfo (TIInt1     ) = Put.putWord8 0x30
putTypeInfo (TIInt2     ) = Put.putWord8 0x34
putTypeInfo (TIInt4     ) = Put.putWord8 0x38
putTypeInfo (TIInt8     ) = Put.putWord8 0x7f
putTypeInfo (TIMoney4   ) = Put.putWord8 0x7a
putTypeInfo (TIMoney8   ) = Put.putWord8 0x3c
putTypeInfo (TIDateTime4) = Put.putWord8 0x3a
putTypeInfo (TIDateTime8) = Put.putWord8 0x3d
putTypeInfo (TIFlt4     ) = Put.putWord8 0x3b
putTypeInfo (TIFlt8     ) = Put.putWord8 0x3e

putTypeInfo (TIBitN      ) = Put.putWord8 0x68 >> Put.putWord8 1
putTypeInfo (TIIntN1     ) = Put.putWord8 0x26 >> Put.putWord8 1
putTypeInfo (TIIntN2     ) = Put.putWord8 0x26 >> Put.putWord8 2
putTypeInfo (TIIntN4     ) = Put.putWord8 0x26 >> Put.putWord8 4
putTypeInfo (TIIntN8     ) = Put.putWord8 0x26 >> Put.putWord8 8
putTypeInfo (TIMoneyN4   ) = Put.putWord8 0x6e >> Put.putWord8 4
putTypeInfo (TIMoneyN8   ) = Put.putWord8 0x6e >> Put.putWord8 8
putTypeInfo (TIDateTimeN4) = Put.putWord8 0x6f >> Put.putWord8 4
putTypeInfo (TIDateTimeN8) = Put.putWord8 0x6f >> Put.putWord8 8
putTypeInfo (TIFltN4     ) = Put.putWord8 0x6d >> Put.putWord8 4
putTypeInfo (TIFltN8     ) = Put.putWord8 0x6d >> Put.putWord8 8

putTypeInfo (TIGUID) = Put.putWord8 0x24 >> Put.putWord8 16
putTypeInfo (TIDecimalN p s) = do
  Put.putWord8 0x6a
  Put.putWord8 $ precisionToLen p -- [TODO] test
  Put.putWord8 p
  Put.putWord8 s
putTypeInfo (TINumericN p s) = do
  Put.putWord8 0x6c
  Put.putWord8 $ precisionToLen p -- [TODO] test
  Put.putWord8 p
  Put.putWord8 s


putTypeInfo (TIChar len) = do -- [TODO] test
  Put.putWord8 0x2f
  Put.putWord8 len
  -- [MEMO] no collation

putTypeInfo (TIVarChar len) = do -- [TODO] test
  Put.putWord8 0x27
  Put.putWord8 len
  -- [MEMO] no collation
  
putTypeInfo (TIBigChar len col) = do
  Put.putWord8 0xaf
  Put.putWord16le len
  putCollation col
  
putTypeInfo (TIBigVarChar len col) = do
  Put.putWord8 0xa7
  Put.putWord16le len
  putCollation col

putTypeInfo (TIText len col) = do
  Put.putWord8 0x23
  Put.putWord32le len
  putCollation col


putTypeInfo (TINChar len col) = do
  Put.putWord8 0xef
  Put.putWord16le len
  putCollation col
  
putTypeInfo (TINVarChar len col) = do
  Put.putWord8 0xe7
  Put.putWord16le len
  putCollation col

putTypeInfo (TINText len col) = do
  Put.putWord8 0x63
  Put.putWord32le len
  putCollation col


putTypeInfo (TIBinary len) = do -- [TODO] test
  Put.putWord8 0x2d
  Put.putWord8 len

putTypeInfo (TIVarBinary len) = do -- [TODO] test
  Put.putWord8 0x25
  Put.putWord8 len
  
putTypeInfo (TIBigBinary len) = do
  Put.putWord8 0xad
  Put.putWord16le len
  
putTypeInfo (TIBigVarBinary len) = do
  Put.putWord8 0xa5
  Put.putWord16le len

putTypeInfo (TIImage len) = do
  Put.putWord8 0x22
  Put.putWord32le len

  
instance Binary TypeInfo where
  put = putTypeInfo
  get = getTypeInfo




type RawBytes = Maybe LB.ByteString

getRawBytes :: TypeInfo -> Get RawBytes
getRawBytes = f
  where

    get8n :: Get RawBytes
    get8n = do
      len <- Get.getWord8
      if len == 0
        then return Nothing
        else Just <$> (Get.getLazyByteString $ fromIntegral len)
    
    get8s :: Get RawBytes
    get8s = do
      len <- Get.getWord8
      if len == 0xff
        then return Nothing
        else Just <$> (Get.getLazyByteString $ fromIntegral len)
    
    get16s :: Get RawBytes
    get16s = do
      len <- Get.getWord16le
      if len == 0xffff
        then return Nothing
        else Just <$> (Get.getLazyByteString $ fromIntegral len)
    
    get32s :: Get RawBytes
    get32s = do
      len <- Get.getWord32le
      if len == 0xffffffff
        then return Nothing
        else Just <$> (Get.getLazyByteString $ fromIntegral len)

    f :: TypeInfo -> Get RawBytes
    f TINull      = return Nothing
    f TIBit       = Just <$> Get.getLazyByteString 1
    f TIInt1      = Just <$> Get.getLazyByteString 1
    f TIInt2      = Just <$> Get.getLazyByteString 2
    f TIInt4      = Just <$> Get.getLazyByteString 4
    f TIInt8      = Just <$> Get.getLazyByteString 8
    f TIMoney4    = Just <$> Get.getLazyByteString 4
    f TIMoney8    = Just <$> Get.getLazyByteString 8
    f TIDateTime4 = Just <$> Get.getLazyByteString 4
    f TIDateTime8 = Just <$> Get.getLazyByteString 8
    f TIFlt4      = Just <$> Get.getLazyByteString 4
    f TIFlt8      = Just <$> Get.getLazyByteString 8

    f TIBitN  = get8n
    f TIIntN1 = get8n
    f TIIntN2 = get8n
    f TIIntN4 = get8n
    f TIIntN8 = get8n
    f TIMoneyN4 = get8n
    f TIMoneyN8 = get8n
    f TIDateTimeN4 = get8n
    f TIDateTimeN8 = get8n
    f TIFltN4 = get8n
    f TIFltN8 = get8n
    
    f TIGUID = get8n
      
    f (TIDecimalN _ _) = get8n
    f (TINumericN _ _) = get8n

    f (TIChar _) = get8s
    f (TIVarChar _) = get8s
    f (TIBigChar _ _) = get16s
    f (TIBigVarChar _ _) = get16s
    f (TIText _ _) = get32s

    f (TINChar _ _) = get16s
    f (TINVarChar _ _) = get16s
    f (TINText _ _) = get32s
             
    f (TIBinary _) = get8s
    f (TIVarBinary _) = get8s
    f (TIBigBinary _) = get16s
    f (TIBigVarBinary _) = get16s
    f (TIImage _) = get32s


putRawBytes :: TypeInfo -> RawBytes -> Put
putRawBytes = g
  where

    put8n :: RawBytes -> Put
    put8n Nothing = Put.putWord8 0
    put8n (Just bs) = do
      Put.putWord8 $ fromIntegral $ LB.length bs
      Put.putLazyByteString  bs
    
    put8s :: RawBytes -> Put
    put8s Nothing = Put.putWord8 0xff
    put8s (Just bs) = do
      Put.putWord8 $ fromIntegral $ LB.length bs
      Put.putLazyByteString  bs
    
    put16s :: RawBytes -> Put
    put16s Nothing = Put.putWord16le 0xffff
    put16s (Just bs) = do
      Put.putWord16le $ fromIntegral $ LB.length bs
      Put.putLazyByteString  bs
    
    put32s :: RawBytes -> Put
    put32s Nothing = Put.putWord32le 0xffffffff
    put32s (Just bs) = do
      Put.putWord32le $ fromIntegral $ LB.length bs
      Put.putLazyByteString  bs
    

    g :: TypeInfo -> RawBytes -> Put
    g TINull _ = return ()
    
    g TIBit Nothing        = error "putRawBytes: Nothing is not convertible to TIBit"
    g TIInt1 Nothing       = error "putRawBytes: Nothing is not convertible to TIInt1"
    g TIInt2 Nothing       = error "putRawBytes: Nothing is not convertible to TIInt2"
    g TIInt4 Nothing       = error "putRawBytes: Nothing is not convertible to TIInt4"
    g TIInt8 Nothing       = error "putRawBytes: Nothing is not convertible to TIInt8"
    g TIMoney4 Nothing     = error "putRawBytes: Nothing is not convertible to TIMoney4"
    g TIMoney8 Nothing     = error "putRawBytes: Nothing is not convertible to TIMoney8"
    g TIDateTime4 Nothing  = error "putRawBytes: Nothing is not convertible to TIDateTime4"
    g TIDateTime8 Nothing  = error "putRawBytes: Nothing is not convertible to TIDateTime8"
    g TIFlt4 Nothing       = error "putRawBytes: Nothing is not convertible to TIFlt4"
    g TIFlt8 Nothing       = error "putRawBytes: Nothing is not convertible to TIFlt8"
    
    g TIBit (Just bs)       = Put.putLazyByteString bs
    g TIInt1 (Just bs)      = Put.putLazyByteString bs
    g TIInt2 (Just bs)      = Put.putLazyByteString bs
    g TIInt4 (Just bs)      = Put.putLazyByteString bs
    g TIInt8 (Just bs)      = Put.putLazyByteString bs
    g TIMoney4 (Just bs)    = Put.putLazyByteString bs
    g TIMoney8 (Just bs)    = Put.putLazyByteString bs
    g TIDateTime4 (Just bs) = Put.putLazyByteString bs
    g TIDateTime8 (Just bs) = Put.putLazyByteString bs
    g TIFlt4 (Just bs)      = Put.putLazyByteString bs
    g TIFlt8 (Just bs)      = Put.putLazyByteString bs

    g TIBitN rb       = put8n rb
    g TIIntN1 rb      = put8n rb
    g TIIntN2 rb      = put8n rb
    g TIIntN4 rb      = put8n rb
    g TIIntN8 rb      = put8n rb
    g TIMoneyN4 rb    = put8n rb
    g TIMoneyN8 rb    = put8n rb
    g TIDateTimeN4 rb = put8n rb
    g TIDateTimeN8 rb = put8n rb
    g TIFltN4 rb      = put8n rb
    g TIFltN8 rb      = put8n rb
    
    g TIGUID rb = put8n rb

    g (TIDecimalN _ _) rb = put8n rb
    g (TINumericN _ _) rb = put8n rb

    g (TIChar _) rb = put8s rb
    g (TIVarChar _) rb = put8s rb
    g (TIBigChar _ _) rb = put16s rb
    g (TIBigVarChar _ _) rb = put16s rb
    g (TIText _ _) rb = put32s rb

    g (TINChar _ _) rb = put16s rb
    g (TINVarChar _ _) rb = put16s rb
    g (TINText _ _) rb = put32s rb

    g (TIBinary _) rb = put8s rb
    g (TIVarBinary _) rb = put8s rb
    g (TIBigBinary _) rb = put16s rb
    g (TIBigVarBinary _) rb = put16s rb
    g (TIImage _) rb = put32s rb



  
withValidNull :: TypeInfo -> (TypeInfo -> a) -> a
withValidNull = f
  where
    f :: TypeInfo -> (TypeInfo -> a) -> a
    f ti@TINull g = g ti
    f ti _ = error $ "withValidNull: " <> (show ti) <> " is not convertible from/to Null"



withValidIntegral :: String -> TypeInfo -> (TypeInfo -> a) -> a
withValidIntegral tn = f
  where
    f :: TypeInfo -> (TypeInfo -> a) -> a
    f ti@TIBit g = g ti
    f ti@TIInt1 g = g ti
    f ti@TIInt2 g = g ti
    f ti@TIInt4 g = g ti
    f ti@TIInt8 g = g ti
    f ti@TIBitN g = g ti
    f ti@TIIntN1 g = g ti
    f ti@TIIntN2 g = g ti 
    f ti@TIIntN4 g = g ti
    f ti@TIIntN8 g = g ti
    f ti _ = error $ "withValidIntegral: " <> (show ti) <> " is not convertible from/to " <> tn

withValidBool = withValidIntegral "Bool"
withValidInt = withValidIntegral "Int"
withValidInteger = withValidIntegral "Integer"

isIntegralN :: TypeInfo -> Bool
isIntegralN = f
  where
    f :: TypeInfo -> Bool
    f TIBitN = True
    f TIIntN1 = True
    f TIIntN2 = True
    f TIIntN4 = True
    f TIIntN8 = True
    f _ = False


getIntegral :: Integral a => TypeInfo -> Get a
getIntegral TIBit  = fromIntegral <$> Get.getWord8
getIntegral TIInt1 = fromIntegral <$> Get.getInt8
getIntegral TIInt2 = fromIntegral <$> Get.getInt16le
getIntegral TIInt4 = fromIntegral <$> Get.getInt32le
getIntegral TIInt8 = fromIntegral <$> Get.getInt64le
getIntegral TIBitN  = getIntegral TIBit
getIntegral TIIntN1 = getIntegral TIInt1
getIntegral TIIntN2 = getIntegral TIInt2
getIntegral TIIntN4 = getIntegral TIInt4
getIntegral TIIntN8 = getIntegral TIInt8


putIntegral :: Integral a => TypeInfo -> a -> Put
putIntegral TIBit  i = Put.putWord8 $ fromIntegral i
putIntegral TIInt1 i = Put.putInt8 $ fromIntegral i
putIntegral TIInt2 i = Put.putInt16le $ fromIntegral i
putIntegral TIInt4 i = Put.putInt32le $ fromIntegral i
putIntegral TIInt8 i = Put.putInt64le $ fromIntegral i
putIntegral TIBitN  i = putIntegral TIBit i
putIntegral TIIntN1 i = putIntegral TIInt1 i
putIntegral TIIntN2 i = putIntegral TIInt2 i
putIntegral TIIntN4 i = putIntegral TIInt4 i
putIntegral TIIntN8 i = putIntegral TIInt8 i



  
withValidMoney :: TypeInfo -> (TypeInfo -> a) -> a
withValidMoney  = f
  where
    f :: TypeInfo -> (TypeInfo -> a) -> a
    f ti@TIMoney4 g = g ti
    f ti@TIMoney8 g = g ti
    f ti@TIMoneyN4 g = g ti
    f ti@TIMoneyN8 g = g ti
    f ti _ = error $ "withValidMoney: " <> (show ti) <> " is not convertible from/to Money"

isMoneyN :: TypeInfo -> Bool
isMoneyN = f
  where
    f :: TypeInfo -> Bool
    f TIMoneyN4 = True
    f TIMoneyN8 = True
    f _ = False
    
getMoney :: TypeInfo -> Get Money
getMoney TIMoney4 = bytesToMoney4 <$> Get.getInt32le
getMoney TIMoney8 = bytesToMoney8 <$> Get.getInt32le <*> Get.getInt32le
getMoney TIMoneyN4 = getMoney TIMoney4
getMoney TIMoneyN8 = getMoney TIMoney8

putMoney :: TypeInfo -> Money -> Put
putMoney TIMoney4 f = Put.putInt32le $ moneyToBytes4 f
putMoney TIMoney8 f = do
  let (m,l) = moneyToBytes8 f
  Put.putInt32le m
  Put.putInt32le l
putMoney TIMoneyN4 f = putMoney TIMoney4 f
putMoney TIMoneyN8 f = putMoney TIMoney8 f


  

withValidUTCTime :: TypeInfo -> (TypeInfo -> a) -> a
withValidUTCTime  = f
  where
    f :: TypeInfo -> (TypeInfo -> a) -> a
    f ti@TIDateTime4 g = g ti
    f ti@TIDateTime8 g = g ti
    f ti@TIDateTimeN4 g = g ti
    f ti@TIDateTimeN8 g = g ti
    f ti _ = error $ "withValidUTCTime: " <> (show ti) <> " is not convertible from/to UTCTime"

isUTCTimeN :: TypeInfo -> Bool
isUTCTimeN = f
  where
    f :: TypeInfo -> Bool
    f TIDateTimeN4 = True
    f TIDateTimeN8 = True
    f _ = False
    
getUTCTime :: TypeInfo -> Get UTCTime
getUTCTime TIDateTime4 = bytesToUtc4 <$> Get.getWord16le <*> Get.getWord16le
getUTCTime TIDateTime8 = bytesToUtc8 <$> Get.getInt32le <*> Get.getWord32le
getUTCTime TIDateTimeN4 = getUTCTime TIDateTime4
getUTCTime TIDateTimeN8 = getUTCTime TIDateTime8

putUTCTime :: TypeInfo -> UTCTime -> Put
putUTCTime TIDateTime4 time = do
  let (wday,wmin) = utcToBytes4 time
  Put.putWord16le wday
  Put.putWord16le wmin
putUTCTime TIDateTime8 time = do
  let (iday,w3hsec) = utcToBytes8 time
  Put.putInt32le iday
  Put.putWord32le w3hsec
putUTCTime TIDateTimeN4 time = putUTCTime TIDateTime4 time
putUTCTime TIDateTimeN8 time = putUTCTime TIDateTime8 time




withValidFloat' :: String -> TypeInfo -> (TypeInfo -> a) -> a
withValidFloat' tn = f
  where
    f :: TypeInfo -> (TypeInfo -> a) -> a
    f ti@TIFlt4 g = g ti
    f ti@TIFlt8 g = g ti
    f ti@TIFltN4 g = g ti
    f ti@TIFltN8 g = g ti
    f ti _ = error $ "withValidFloat': " <> (show ti) <> " is not convertible from/to " <> tn

withValidFloat = withValidFloat' "Float"
withValidDouble = withValidFloat' "Double"

isFloatN :: TypeInfo -> Bool
isFloatN = f
  where
    f :: TypeInfo -> Bool
    f TIFltN4 = True
    f TIFltN8 = True
    f _ = False
    
getFloat :: Fractional a => TypeInfo -> Get a
getFloat TIFlt4 = realToFrac . wordToFloat <$> Get.getWord32le
getFloat TIFlt8 = realToFrac . wordToDouble <$> Get.getWord64le
getFloat TIFltN4 = getFloat TIFlt4
getFloat TIFltN8 = getFloat TIFlt8

putFloat :: Real a => TypeInfo -> a -> Put
putFloat TIFlt4 f = Put.putWord32le $ floatToWord $ realToFrac f
putFloat TIFlt8 f = Put.putWord64le $ doubleToWord $ realToFrac f
putFloat TIFltN4 f = putFloat TIFlt4 f
putFloat TIFltN8 f = putFloat TIFlt8 f




withValidDecimal :: TypeInfo -> (TypeInfo -> a) -> a
withValidDecimal  = f
  where
    f :: TypeInfo -> (TypeInfo -> a) -> a
    f ti@(TIDecimalN _ _) g = g ti
    f ti@(TINumericN _ _) g = g ti
    f ti _ = error $ "withValidDecimal: " <> (show ti) <> " is not convertible from/to Decimal"

-- https://msdn.microsoft.com/en-us/library/ee780893.aspx
-- [MEMO] sign byte + signed bytes
getDecimal :: Int -> Scale -> Get Decimal
getDecimal len s = 
  bytesToDecimal s <$> Get.getWord8 <*> (Get.getByteString $ fromIntegral $ len -1)

putDecimal :: TypeInfo -> Decimal -> Put
putDecimal (TIDecimalN p _) dec = do -- [TODO] test
  let (s,bs) = decimalToBytes p dec
  Put.putWord8 s
  Put.putByteString bs
putDecimal (TINumericN p s) dec = putDecimal (TIDecimalN p s) dec




withValidUUID :: TypeInfo -> (TypeInfo -> a) -> a
withValidUUID  = f
  where
    f :: TypeInfo -> (TypeInfo -> a) -> a
    f ti@TIGUID g = g ti
    f ti _ = error $ "withValidUUID: " <> (show ti) <> " is not convertible from/to UUID"




withValidByteString :: TypeInfo -> (TypeInfo -> a) -> a
withValidByteString  = f
  where
    f :: TypeInfo -> (TypeInfo -> a) -> a
    f ti@(TIChar _) g = g ti
    f ti@(TIVarChar _) g = g ti
    f ti@(TIBigChar _ _) g = g ti
    f ti@(TIBigVarChar _ _) g = g ti
    f ti@(TIText _ _) g = g ti
    f ti@(TIBinary _) g = g ti
    f ti@(TIVarBinary _) g = g ti
    f ti@(TIBigBinary _) g = g ti
    f ti@(TIBigVarBinary _) g = g ti
    f ti@(TIImage _) g = g ti
    f ti _ = error $ "withValidByteString: " <> (show ti) <> " is not convertible from/to ByteString"
    



withValidText' :: String -> TypeInfo -> (TypeInfo -> a) -> a
withValidText' tn = f
  where
    f :: TypeInfo -> (TypeInfo -> a) -> a
    f ti@(TINChar _ _) g = g ti
    f ti@(TINVarChar _ _) g = g ti
    f ti@(TINText _ _) g = g ti
    f ti _ = error $ "withValidText: " <> (show ti) <> " is not convertible from/to " <> tn

withValidText = withValidText' "Text"
withValidString = withValidText' "String"



runGet :: Get a -> LB.ByteString -> a
runGet f bs = Get.runGet f bs

runPut :: Put -> LB.ByteString
runPut f = Put.runPut f


runGetBool :: TypeInfo -> LB.ByteString -> Bool
runGetBool ti bs = (/=0) $ runGet (getIntegral ti) bs

runPutBool :: TypeInfo -> Bool -> LB.ByteString
runPutBool ti b = runPut $ putIntegral ti $ if b then 1 else 0



-- [TODO] check nullable flag

-- | [\[MS-TDS\] 2.2.5.5.1 System Data Type Values](https://docs.microsoft.com/en-us/openspecs/windows_protocols/ms-tds/5773bd3e-a8cf-45cc-a058-3fd3ec3a8aff)
class Data a where
  fromRawBytes :: TypeInfo -> RawBytes -> a
  toRawBytes :: TypeInfo -> a -> RawBytes


instance Data Null where
  fromRawBytes ti rb = withValidNull ti $ \_ -> f rb
    where
      f Nothing = Null
      f _ = error "Null.fromRawBytes: non-Null value is not convertible to Null"
  toRawBytes ti _ = withValidNull ti $ \_ -> Nothing

instance Data Bool where
  fromRawBytes ti (Just bs) = withValidBool ti $ \vt -> runGetBool vt bs
  fromRawBytes ti Nothing = withValidBool ti $ \_ -> error "Bool.fromRawBytes: Null value is not convertible to Bool"
  toRawBytes ti b = withValidBool ti $ \vt -> Just $ runPutBool vt b

instance Data Int where
  fromRawBytes ti (Just bs) = withValidInt ti $ \vt -> runGet (getIntegral vt) bs
  fromRawBytes ti Nothing = withValidInt ti $ \_ -> error "Int.fromRawBytes: Null value is not convertible to Int"
  toRawBytes ti i = withValidInt ti $ \vt -> Just $ runPut $ putIntegral vt i

instance Data Integer where
  fromRawBytes ti (Just bs) = withValidInteger ti $ \vt -> runGet (getIntegral vt) bs
  fromRawBytes ti Nothing = withValidInteger ti $ \_ -> error "Integer.fromRawBytes: Null value is not convertible to Integer"
  toRawBytes ti i = withValidInteger ti $ \vt -> Just $ runPut $ putIntegral vt i

instance Data Money where
  fromRawBytes ti (Just bs) = withValidMoney ti $ \vt -> runGet (getMoney vt) bs
  fromRawBytes ti Nothing = withValidMoney ti $ \_-> error "Money.fromRawBytes: Null value is not convertible to Money"
  toRawBytes ti m = withValidMoney ti $ \vt -> Just $ runPut $ putMoney vt m
    
instance Data UTCTime where
  fromRawBytes ti (Just bs) = withValidUTCTime ti $ \vt -> runGet (getUTCTime vt) bs
  fromRawBytes ti Nothing = withValidUTCTime ti $ \vt -> error "UTCTime.fromRawBytes: Null value is not convertible to UTCTime"
  toRawBytes ti dt = withValidUTCTime ti $ \vt -> Just $ runPut $ putUTCTime vt dt

instance Data Float where
  fromRawBytes ti (Just bs) = withValidFloat ti $ \vt -> runGet (getFloat ti) bs
  fromRawBytes ti Nothing = withValidFloat ti $ \_ -> error "Float.fromRawBytes: Null value is not convertible to Float"
  toRawBytes ti f = withValidFloat ti $ \vt -> Just $ runPut $ putFloat vt f

instance Data Double where
  fromRawBytes ti (Just bs) = withValidDouble ti $ \vt -> runGet (getFloat ti) bs
  fromRawBytes ti Nothing = withValidDouble ti $ \_ -> error "Double.fromRawBytes: Null value is not convertible to Double"
  toRawBytes ti f = withValidDouble ti $ \vt -> Just $ runPut $ putFloat vt f

instance Data Decimal where
  fromRawBytes ti (Just bs) = withValidDecimal ti $ \vt ->
    runGet (getDecimal (fromIntegral $ LB.length bs) (scale vt)) bs
    where
      scale :: TypeInfo -> Scale
      scale (TIDecimalN _ s) = s
      scale (TINumericN _ s) = s
  fromRawBytes ti Nothing = withValidDecimal ti $ \_ -> error "Decimal.fromRawBytes: Null value is not convertible to Decimal"
  toRawBytes ti dec = withValidDecimal ti $ \_ -> Just $ runPut $ putDecimal ti dec
    
instance Data UUID where
  fromRawBytes ti (Just bs) = withValidUUID ti $ \_ -> case UUID.fromByteString bs of
                                                         Nothing -> error "UUID.fromRawBytes: UUID.fromBtyteString error"
                                                         Just (uuid) -> uuid
  fromRawBytes ti Nothing = withValidUUID ti $ \_ -> error "UUID.fromRawBytes: Null value is not convertible to UUID"
  toRawBytes ti uuid = withValidUUID ti $ \_ -> Just $ UUID.toByteString uuid

instance Data B.ByteString where
  fromRawBytes ti (Just bs) = withValidByteString ti $ \_ -> LB.toStrict bs
  fromRawBytes ti Nothing = withValidByteString ti $ \_ -> error "ByteString.fromRawBytes: Null value is not convertible to ByteString"
  toRawBytes ti bs = withValidByteString ti $ \_ -> Just $ LB.fromStrict bs

instance Data T.Text where
  fromRawBytes ti (Just bs) = withValidText ti $ \_ -> T.decodeUtf16LE $ LB.toStrict bs
  fromRawBytes ti Nothing = withValidText ti $ \_ -> error "Text.fromRawBytes: Null value is not convertible to Text"
  toRawBytes ti t = withValidText ti $ \_ -> Just $ LB.fromStrict $ T.encodeUtf16LE t

instance Data LB.ByteString where
  fromRawBytes ti (Just bs) = withValidByteString ti $ \_ -> bs
  fromRawBytes ti Nothing = withValidByteString ti $ \_ -> error "ByteString.fromRawBytes: Null value is not convertible to ByteString"
  toRawBytes ti bs = withValidByteString ti $ \_ -> Just bs

instance Data LT.Text where
  fromRawBytes ti (Just bs) = withValidText ti $ \_ -> LT.decodeUtf16LE bs
  fromRawBytes ti Nothing = withValidText ti $ \_ -> error "Text.fromRawBytes: Null value is not convertible to Text"
  toRawBytes ti t = withValidText ti $ \_ -> Just $ LT.encodeUtf16LE t

instance Data String where
  fromRawBytes ti (Just bs) = withValidString ti $ \_ -> LT.unpack $ LT.decodeUtf16LE bs
  fromRawBytes ti Nothing = withValidString ti $ \_ -> error "String.fromRawBytes: Null value is not convertible to String"
  toRawBytes ti s = withValidString ti $ \_ -> Just $ LT.encodeUtf16LE $ LT.pack s



instance Data (Maybe Bool) where
  fromRawBytes ti rb = withValidBool ti $ \vt -> runGetBool vt <$> rb
  toRawBytes ti b = withValidBool ti $ \vt ->
                    case b of
                      Nothing | (not . isIntegralN) vt -> error $ "(Maybe Bool).toRawBytes: Nothing is not convertible to " <> (show vt)
                      _ -> runPutBool vt <$> b

instance Data (Maybe Int) where
  fromRawBytes ti rb = withValidInt ti $ \vt -> (runGet (getIntegral vt)) <$> rb
  toRawBytes ti i = withValidInt ti $ \vt -> 
                    case i of
                      Nothing | (not . isIntegralN) vt -> error $ "(Maybe Int).toRawBytes: Nothing is not convertible to " <> (show vt)
                      _ -> runPut . (putIntegral vt) <$> i

instance Data (Maybe Integer) where
  fromRawBytes ti rb = withValidInteger ti $ \vt -> (runGet (getIntegral vt)) <$> rb
  toRawBytes ti i = withValidInteger ti $ \vt->
                    case i of
                      Nothing | (not . isIntegralN) vt -> error $ "(Maybe Integer).toRawBytes: Nothing is not convertible to " <> (show vt)
                      _ -> runPut . (putIntegral vt) <$> i

instance Data (Maybe Money) where
  fromRawBytes ti rb = withValidMoney ti $ \vt -> (runGet (getMoney vt)) <$> rb
  toRawBytes ti m = withValidMoney ti $ \vt -> 
                    case m of
                      Nothing | (not . isMoneyN) vt -> error $ "(Maybe Money).toRawBytes: Nothing is not convertible to " <> (show vt)
                      _ -> runPut . (putMoney vt) <$> m

instance Data (Maybe UTCTime) where
  fromRawBytes ti rb = withValidUTCTime ti $ \vt -> (runGet (getUTCTime vt)) <$> rb
  toRawBytes ti dt = withValidUTCTime ti $ \vt ->
                     case dt of
                       Nothing | (not . isUTCTimeN) vt -> error $ "(Maybe UTCTime).toRawBytes: Nothing is not convertible to " <> (show vt)
                       _ -> runPut . (putUTCTime vt) <$> dt

instance Data (Maybe Float) where
  fromRawBytes ti rb = withValidFloat ti $ \vt -> (runGet (getFloat vt)) <$> rb
  toRawBytes ti f = withValidFloat ti $ \vt ->
                    case f of
                      Nothing | (not . isFloatN) vt -> error $ "(Maybe Float).toRawBytes: Nothing is not convertible to " <> (show vt)
                      _ -> runPut . (putFloat vt) <$> f

instance Data (Maybe Double) where
  fromRawBytes ti rb = withValidDouble ti $ \vt -> (runGet (getFloat vt)) <$> rb
  toRawBytes ti f = withValidDouble ti $ \vt ->
                    case f of
                      Nothing | (not . isFloatN) vt -> error $ "(Maybe Double).toRawBytes: Nothing is not convertible to " <> (show vt)
                      _ -> runPut . (putFloat vt) <$> f

instance Data (Maybe Decimal) where
  fromRawBytes ti rb = withValidDecimal ti $ \vt ->
    (\bs -> runGet (getDecimal (fromIntegral $ LB.length bs) (scale vt)) bs) <$> rb
    where
      scale :: TypeInfo -> Scale
      scale (TIDecimalN _ s) = s
      scale (TINumericN _ s) = s
  toRawBytes ti dec = withValidDecimal ti $ \_ -> runPut . putDecimal ti <$>  dec

instance Data (Maybe UUID) where
  fromRawBytes ti rb = withValidUUID ti $ \_ -> f <$> rb
    where
      f :: LB.ByteString -> UUID
      f bs = case UUID.fromByteString bs of
               Nothing -> error "(Maybe UUID).fromRawBytes: UUID.fromBtyteString error"
               Just (uuid) -> uuid
  toRawBytes ti m = withValidUUID ti $ \_ -> UUID.toByteString <$> m

instance Data (Maybe B.ByteString) where
  fromRawBytes ti rb = withValidByteString ti $ \_ -> LB.toStrict <$> rb
  toRawBytes ti bs = withValidByteString ti $ \_ -> LB.fromStrict <$> bs
  
instance Data (Maybe T.Text) where
  fromRawBytes ti rb = withValidText ti $ \_ -> T.decodeUtf16LE . LB.toStrict <$> rb 
  toRawBytes ti t = withValidText ti $ \_ -> LB.fromStrict . T.encodeUtf16LE <$> t

instance Data (Maybe LB.ByteString) where
  fromRawBytes ti rb = withValidByteString ti $ \_ -> rb
  toRawBytes ti bs = withValidByteString ti $ \_ -> bs
  
instance Data (Maybe LT.Text) where
  fromRawBytes ti rb = withValidText ti $ \_ -> LT.decodeUtf16LE <$> rb 
  toRawBytes ti t = withValidText ti $ \_ -> LT.encodeUtf16LE <$> t

instance Data (Maybe String) where
  fromRawBytes ti rb = withValidString ti $ \_ -> LT.unpack . LT.decodeUtf16LE <$> rb 
  toRawBytes ti s = withValidString ti $ \_ -> LT.encodeUtf16LE . LT.pack <$> s


