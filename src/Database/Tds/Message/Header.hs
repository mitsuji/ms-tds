{-# OPTIONS_HADDOCK hide #-}
-- Packet Header:   https://msdn.microsoft.com/en-us/library/dd340948.aspx

module Database.Tds.Message.Header ( packetSize
                                   , tdsVersion
                                   , Header (..)
                                   , putMessage
                                   , getMessage
                                   ) where

import Control.Applicative((<$>))

import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Builder as BB

import Data.Word (Word8(..),Word16(..),Word32(..),Word64(..))
import Data.Int (Int8(..),Int16(..),Int32(..),Int64(..))

import Data.Binary (Put(..),Get(..),Binary(..))
import qualified Data.Binary.Put as Put
import qualified Data.Binary.Get as Get

import Control.Monad.Writer (WriterT(..),runWriterT,tell)
import Control.Monad.Trans (lift)


type Type = Word8
type Status = Word8
type Length = Word16
type SPID = Word16
type PacketID = Word8
type Window = Word8

data Header = Header !Type !Status !Length !SPID !PacketID !Window


headerLength :: Integral a => a
headerLength = fromIntegral 8

packetSize :: Integral a => a
packetSize = fromIntegral 4096

tdsVersion :: Word32
tdsVersion = 0x71000001

-- [MEMO]
-- tds70Version = 0x70000000
-- tds71Version = 0x71000001
-- tds72Version = 0x72090002
-- tds73Version = 0x730B0003
-- tds74Version = 0x74000004



-- https://msdn.microsoft.com/en-us/library/dd340948.aspx
putHeader :: Header -> Put
putHeader (Header pt st len spid pcid win) = do
  Put.putWord8 pt      -- packet type
  Put.putWord8 st      -- packet status -- [TODO] flags
  Put.putWord16be $ fromIntegral $ len -- packet len
  Put.putWord16be spid -- SPID
  Put.putWord8 pcid    -- PacketID
  Put.putWord8 win     -- Window

-- https://msdn.microsoft.com/en-us/library/dd340948.aspx
getHeader :: Get Header
getHeader = do
  pt   <- Get.getWord8    -- packet type
  st   <- Get.getWord8    -- packet status -- [TODO] flags
  len  <- Get.getWord16be -- packet len
  spid <- Get.getWord16be -- SPID
  pcid <- Get.getWord8    -- PacketIK
  win  <- Get.getWord8    -- Window
  return $ Header pt st len spid pcid win


instance Binary Header where
  put = putHeader
  get = getHeader



putMessage :: Word8 -> LB.ByteString -> Put
putMessage pt bs = mapM_ f $ split (packetSize -headerLength) bs
  where
    f :: (Bool,LB.ByteString) -> Put
    f (isLast,bs) = do
      let
        len = (fromIntegral $ LB.length bs) + headerLength
        flg = if isLast then 0x01 else 0x00 -- last flag
      put $ Header pt flg len 0 0 0
      Put.putLazyByteString bs


    split :: Int64 -> LB.ByteString -> [(Bool,LB.ByteString)]
    split len lbs =
      let
        (lbs',rem) = LB.splitAt len lbs
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


