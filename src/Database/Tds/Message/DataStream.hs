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

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

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




type RawBytes = Maybe B.ByteString

getRawBytes :: TypeInfo -> Get RawBytes
getRawBytes = f
  where

    get8n :: Get RawBytes
    get8n = do
      len <- Get.getWord8
      if len == 0
        then return Nothing
        else Just <$> (Get.getByteString $ fromIntegral len)
    
    get8s :: Get RawBytes
    get8s = do
      len <- Get.getWord8
      if len == 0xff
        then return Nothing
        else Just <$> (Get.getByteString $ fromIntegral len)
    
    get16s :: Get RawBytes
    get16s = do
      len <- Get.getWord16le
      if len == 0xffff
        then return Nothing
        else Just <$> (Get.getByteString $ fromIntegral len)
    
    get32s :: Get RawBytes
    get32s = do
      len <- Get.getWord32le
      if len == 0xffffffff
        then return Nothing
        else Just <$> (Get.getByteString $ fromIntegral len)

    f :: TypeInfo -> Get RawBytes
    f TINull      = return Nothing
    f TIBit       = Just <$> Get.getByteString 1
    f TIInt1      = Just <$> Get.getByteString 1
    f TIInt2      = Just <$> Get.getByteString 2
    f TIInt4      = Just <$> Get.getByteString 4
    f TIInt8      = Just <$> Get.getByteString 8
    f TIMoney4    = Just <$> Get.getByteString 4
    f TIMoney8    = Just <$> Get.getByteString 8
    f TIDateTime4 = Just <$> Get.getByteString 4
    f TIDateTime8 = Just <$> Get.getByteString 8
    f TIFlt4      = Just <$> Get.getByteString 4
    f TIFlt8      = Just <$> Get.getByteString 8

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
      Put.putWord8 $ fromIntegral $ B.length bs
      Put.putByteString  bs
    
    put8s :: RawBytes -> Put
    put8s Nothing = Put.putWord8 0xff
    put8s (Just bs) = do
      Put.putWord8 $ fromIntegral $ B.length bs
      Put.putByteString  bs
    
    put16s :: RawBytes -> Put
    put16s Nothing = Put.putWord16le 0xffff
    put16s (Just bs) = do
      Put.putWord16le $ fromIntegral $ B.length bs
      Put.putByteString  bs
    
    put32s :: RawBytes -> Put
    put32s Nothing = Put.putWord32le 0xffffffff
    put32s (Just bs) = do
      Put.putWord32le $ fromIntegral $ B.length bs
      Put.putByteString  bs
    

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
    
    g TIBit (Just bs)       = Put.putByteString bs
    g TIInt1 (Just bs)      = Put.putByteString bs
    g TIInt2 (Just bs)      = Put.putByteString bs
    g TIInt4 (Just bs)      = Put.putByteString bs
    g TIInt8 (Just bs)      = Put.putByteString bs
    g TIMoney4 (Just bs)    = Put.putByteString bs
    g TIMoney8 (Just bs)    = Put.putByteString bs
    g TIDateTime4 (Just bs) = Put.putByteString bs
    g TIDateTime8 (Just bs) = Put.putByteString bs
    g TIFlt4 (Just bs)      = Put.putByteString bs
    g TIFlt8 (Just bs)      = Put.putByteString bs

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



  
validNull :: TypeInfo -> a -> a
validNull  = f
  where
    f :: TypeInfo -> a -> a
    f TINull x = x
    f ti _ = error $ "validNull: " <> (show ti) <> " is not convertible from/to Null"



validIntegral :: String -> TypeInfo -> a -> a
validIntegral ht ti x = f ti x
  where
    f :: TypeInfo -> a -> a
    f TIBit x = x
    f TIInt1 x = x
    f TIInt2 x = x
    f TIInt4 x = x
    f TIInt8 x = x
    f TIBitN x = x
    f TIIntN1 x = x
    f TIIntN2 x = x 
    f TIIntN4 x = x
    f TIIntN8 x = x
    f _ _ = error $ "validIntegral: " <> (show ti) <> " is not convertible from/to " <> ht

validBool = validIntegral "Bool"
validInt = validIntegral "Int"
validInteger = validIntegral "Integer"
validMaybeBool = validIntegral "(Maybe Bool)"
validMaybeInt = validIntegral "(Maybe Int)"
validMaybeInteger = validIntegral "(Maybe Integer)"

isIntegralN :: TypeInfo -> Bool
isIntegralN = f
  where
    f :: TypeInfo -> Bool
    f TIBit = False
    f TIInt1 = False
    f TIInt2 = False
    f TIInt4 = False
    f TIInt8 = False
    f TIBitN = True
    f TIIntN1 = True
    f TIIntN2 = True
    f TIIntN4 = True
    f TIIntN8 = True


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



  
validMoney :: TypeInfo -> a -> a
validMoney  = f
  where
    f :: TypeInfo -> a -> a
    f TIMoney4 x = x
    f TIMoney8 x = x
    f TIMoneyN4 x = x
    f TIMoneyN8 x = x
    f ti _ = error $ "validMoney: " <> (show ti) <> " is not convertible from/to Money"

isMoneyN :: TypeInfo -> Bool
isMoneyN = f
  where
    f :: TypeInfo -> Bool
    f TIMoney4 = False
    f TIMoney8 = False
    f TIMoneyN4 = True
    f TIMoneyN8 = True
    
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


  

validUTCTime :: TypeInfo -> a -> a
validUTCTime  = f
  where
    f :: TypeInfo -> a -> a
    f TIDateTime4 x = x
    f TIDateTime8 x = x
    f TIDateTimeN4 x = x
    f TIDateTimeN8 x = x
    f ti _ = error $ "validUTCTime: " <> (show ti) <> " is not convertible from/to UTCTime"

isUTCTimeN :: TypeInfo -> Bool
isUTCTimeN = f
  where
    f :: TypeInfo -> Bool
    f TIDateTime4 = False
    f TIDateTime8 = False
    f TIDateTimeN4 = True
    f TIDateTimeN8 = True
    
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




validFloat' :: String -> TypeInfo -> a -> a
validFloat' hn ti x = f ti x
  where
    f :: TypeInfo -> a -> a
    f TIFlt4 x = x
    f TIFlt8 x = x
    f TIFltN4 x = x
    f TIFltN8 x = x
    f ti _ = error $ "validFloat': " <> (show ti) <> " is not convertible from/to " <> hn

validFloat = validFloat' "Float"
validDouble = validFloat' "Double"
validMaybeFloat = validFloat' "(Maybe Float)"
validMaybeDouble = validFloat' "(Maybe Double)"

isFloatN :: TypeInfo -> Bool
isFloatN = f
  where
    f :: TypeInfo -> Bool
    f TIFlt4 = False
    f TIFlt8 = False
    f TIFltN4 = True
    f TIFltN8 = True
    
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




validDecimal :: TypeInfo -> a -> a
validDecimal  = f
  where
    f :: TypeInfo -> a -> a
    f (TIDecimalN _ _) x = x
    f (TINumericN _ _) x = x
    f ti _ = error $ "validDecimal: " <> (show ti) <> " is not convertible from/to Decimal"

-- https://msdn.microsoft.com/en-us/library/ee780893.aspx
-- [MEMO] sign byte + signed bytes
getDecimal :: Int -> Precision -> Scale -> Get Decimal
getDecimal len p s = 
  bytesToDecimal p s <$> Get.getWord8 <*> (Get.getByteString $ fromIntegral $ len -1)

putDecimal :: Decimal -> Put
putDecimal dec = do -- [TODO] test
  let (s,bs) = decimalToBytes dec
  Put.putWord8 s
  Put.putByteString bs




validUUID :: TypeInfo -> a -> a
validUUID  = f
  where
    f :: TypeInfo -> a -> a
    f TIGUID x = x
    f ti _ = error $ "validUUID: " <> (show ti) <> " is not convertible from/to UUID"




validByteString :: TypeInfo -> a -> a
validByteString  = f
  where
    f :: TypeInfo -> a -> a
    f (TIChar _) x = x
    f (TIVarChar _) x = x
    f (TIBigChar _ _) x = x
    f (TIBigVarChar _ _) x = x
    f (TIText _ _) x = x
    f (TIBinary _) x = x
    f (TIVarBinary _) x = x
    f (TIBigBinary _) x = x
    f (TIBigVarBinary _) x = x
    f (TIImage _) x = x
    f ti _ = error $ "validByteString: " <> (show ti) <> " is not convertible from/to ByteString"
    



validText :: TypeInfo -> a -> a
validText  = f
  where
    f :: TypeInfo -> a -> a
    f (TINChar _ _) x = x
    f (TINVarChar _ _) x = x
    f (TINText _ _) x= x
    f ti _ = error $ "validText: " <> (show ti) <> " is not convertible from/to Text"
    



runGet :: Get a -> B.ByteString -> a
runGet f bs = Get.runGet f $ LB.fromStrict bs

runPut :: Put -> B.ByteString
runPut f = LB.toStrict $ Put.runPut f


runGetBool :: TypeInfo -> B.ByteString -> Bool
runGetBool ti bs = (/=0) $ runGet (getIntegral ti) bs

runPutBool :: TypeInfo -> Bool -> B.ByteString
runPutBool ti b = runPut $ putIntegral ti $ if b then 1 else 0



-- [TODO] check nullable flag

class Data a where
  fromRawBytes :: TypeInfo -> RawBytes -> a
  toRawBytes :: TypeInfo -> a -> RawBytes


instance Data Null where
  fromRawBytes ti rb = validNull ti $ f rb
    where
      f Nothing = Null
      f _ = error "Null.fromRawBytes: non-Null value is not convertible to Null"
  toRawBytes ti _ = validNull ti $ Nothing

instance Data Bool where
  fromRawBytes ti (Just bs) = validBool ti $ runGetBool ti bs
  fromRawBytes ti Nothing = validBool ti $ error "Bool.fromRawBytes: Null value is not convertible to Bool"
  toRawBytes ti b = validBool ti $ Just $ runPutBool ti b

instance Data Int where
  fromRawBytes ti (Just bs) = validInt ti $ runGet (getIntegral ti) bs
  fromRawBytes ti Nothing = validInt ti $ error "Int.fromRawBytes: Null value is not convertible to Int"
  toRawBytes ti i = validInt ti $ Just $ runPut $ putIntegral ti i

instance Data Integer where
  fromRawBytes ti (Just bs) = validInteger ti $ runGet (getIntegral ti) bs
  fromRawBytes ti Nothing = validInteger ti $ error "Integer.fromRawBytes: Null value is not convertible to Integer"
  toRawBytes ti i = validInteger ti $ Just $ runPut $ putIntegral ti i

instance Data Money where
  fromRawBytes ti (Just bs) = validMoney ti $ runGet (getMoney ti) bs
  fromRawBytes ti Nothing = validMoney ti $ error "Money.fromRawBytes: Null value is not convertible to Money"
  toRawBytes ti m = validMoney ti $ Just $ runPut $ putMoney ti m
    
instance Data UTCTime where
  fromRawBytes ti (Just bs) = validUTCTime ti $ runGet (getUTCTime ti) bs
  fromRawBytes ti Nothing = validUTCTime ti $ error "UTCTime.fromRawBytes: Null value is not convertible to UTCTime"
  toRawBytes ti dt = validUTCTime ti $ Just $ runPut $ putUTCTime ti dt

instance Data Float where
  fromRawBytes ti (Just bs) = validFloat ti $ runGet (getFloat ti) bs
  fromRawBytes ti Nothing = validFloat ti $ error "Float.fromRawBytes: Null value is not convertible to Float"
  toRawBytes ti f = validFloat ti $ Just $ runPut $ putFloat ti f

instance Data Double where
  fromRawBytes ti (Just bs) = validDouble ti $ runGet (getFloat ti) bs
  fromRawBytes ti Nothing = validDouble ti $ error "Double.fromRawBytes: Null value is not convertible to Double"
  toRawBytes ti f = validDouble ti $ Just $ runPut $ putFloat ti f

instance Data Decimal where
  fromRawBytes ti (Just bs) = validDecimal ti $
    let (p,s) = ps ti
    in runGet (getDecimal (B.length bs) p s) bs
    where
      ps :: TypeInfo -> (Precision,Scale)
      ps (TIDecimalN p s) = (p,s)
      ps (TINumericN p s) = (p,s)
  fromRawBytes ti Nothing = validDecimal ti $ error "Decimal.fromRawBytes: Null value is not convertible to Decimal"
  toRawBytes ti dec = validDecimal ti $ Just $ runPut $ putDecimal dec
    
instance Data UUID where
  fromRawBytes ti (Just bs) = validUUID ti $ case UUID.fromByteString $ LB.fromStrict bs of
                                               Nothing -> error "UUID.fromRawBytes: UUID.fromBtyteString error"
                                               Just (uuid) -> uuid
  fromRawBytes ti Nothing = validUUID ti $ error "UUID.fromRawBytes: Null value is not convertible to UUID"
  toRawBytes ti uuid = validUUID ti $ Just $ LB.toStrict $ UUID.toByteString uuid

instance Data B.ByteString where
  fromRawBytes ti (Just bs) = validByteString ti $ bs
  fromRawBytes ti Nothing = validByteString ti $ error "ByteString.fromRawBytes: Null value is not convertible to ByteString"
  toRawBytes ti bs = validByteString ti $ Just $ bs

instance Data T.Text where
  fromRawBytes ti (Just bs) = validText ti $ T.decodeUtf16LE bs
  fromRawBytes ti Nothing = validText ti $ error "Text.fromRawBytes: Null value is not convertible to Text"
  toRawBytes ti t = validText ti $ Just $ T.encodeUtf16LE t



instance Data (Maybe Bool) where
  fromRawBytes ti rb = validMaybeBool ti $ runGetBool ti <$> rb
  toRawBytes ti b = validMaybeBool ti $
                    case b of
                      Nothing | (not . isIntegralN) ti -> error $ "(Maybe Bool).toRawBytes: Nothing is not convertible to " <> (show ti)
                      _ -> runPutBool ti <$> b

instance Data (Maybe Int) where
  fromRawBytes ti rb = validMaybeInt ti $ (runGet (getIntegral ti)) <$> rb
  toRawBytes ti i = validMaybeInt ti $
                    case i of
                      Nothing | (not . isIntegralN) ti -> error $ "(Maybe Int).toRawBytes: Nothing is not convertible to " <> (show ti)
                      _ -> runPut . (putIntegral ti) <$> i

instance Data (Maybe Integer) where
  fromRawBytes ti rb = validMaybeInteger ti $ (runGet (getIntegral ti)) <$> rb
  toRawBytes ti i = validMaybeInteger ti $
                    case i of
                      Nothing | (not . isIntegralN) ti -> error $ "(Maybe Integer).toRawBytes: Nothing is not convertible to " <> (show ti)
                      _ -> runPut . (putIntegral ti) <$> i

instance Data (Maybe Money) where
  fromRawBytes ti rb = validMoney ti $ (runGet (getMoney ti)) <$> rb
  toRawBytes ti m = validMoney ti $
                    case m of
                      Nothing | (not . isMoneyN) ti -> error $ "(Maybe Money).toRawBytes: Nothing is not convertible to " <> (show ti)
                      _ -> runPut . (putMoney ti) <$> m

instance Data (Maybe UTCTime) where
  fromRawBytes ti rb = validUTCTime ti $ (runGet (getUTCTime ti)) <$> rb
  toRawBytes ti dt = validUTCTime ti $
                     case dt of
                       Nothing | (not . isUTCTimeN) ti -> error $ "(Maybe UTCTime).toRawBytes: Nothing is not convertible to " <> (show ti)
                       _ -> runPut . (putUTCTime ti) <$> dt

instance Data (Maybe Float) where
  fromRawBytes ti rb = validMaybeFloat ti $ (runGet (getFloat ti)) <$> rb
  toRawBytes ti f = validMaybeFloat ti $
                    case f of
                      Nothing | (not . isFloatN) ti -> error $ "(Maybe Float).toRawBytes: Nothing is not convertible to " <> (show ti)
                      _ -> runPut . (putFloat ti) <$> f

instance Data (Maybe Double) where
  fromRawBytes ti rb = validMaybeDouble ti $ (runGet (getFloat ti)) <$> rb
  toRawBytes ti f = validMaybeDouble ti $
                    case f of
                      Nothing | (not . isFloatN) ti -> error $ "(Maybe Double).toRawBytes: Nothing is not convertible to " <> (show ti)
                      _ -> runPut . (putFloat ti) <$> f

instance Data (Maybe Decimal) where
  fromRawBytes ti rb = validDecimal ti $
    let (p,s) = ps ti
    in (\bs -> runGet (getDecimal (B.length bs) p s) bs) <$> rb
    where
      ps :: TypeInfo -> (Precision,Scale)
      ps (TIDecimalN p s) = (p,s)
      ps (TINumericN p s) = (p,s)
  toRawBytes ti dec = validDecimal ti $ runPut . putDecimal <$> dec

instance Data (Maybe UUID) where
  fromRawBytes ti rb = validUUID ti $ f <$> rb
    where
      f :: B.ByteString -> UUID
      f bs = case UUID.fromByteString $ LB.fromStrict bs of
               Nothing -> error "(Maybe UUID).fromRawBytes: UUID.fromBtyteString error"
               Just (uuid) -> uuid
  toRawBytes ti m = validUUID ti $ (LB.toStrict . UUID.toByteString) <$> m

instance Data (Maybe B.ByteString) where
  fromRawBytes ti rb = validByteString ti $ rb
  toRawBytes ti bs = validByteString ti $ bs
  
instance Data (Maybe T.Text) where
  fromRawBytes ti rb = validText ti $ T.decodeUtf16LE <$> rb 
  toRawBytes ti t = validText ti $ T.encodeUtf16LE <$> t


