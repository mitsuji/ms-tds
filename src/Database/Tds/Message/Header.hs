{-# OPTIONS_HADDOCK hide #-}
-- Packet Header:   https://msdn.microsoft.com/en-us/library/dd340948.aspx

module Database.Tds.Message.Header ( Header (..)
                                   ) where

import Data.Word (Word8(..),Word16(..),Word32(..),Word64(..))
import Data.Int (Int8(..),Int16(..),Int32(..),Int64(..))

import Data.Binary (Put(..),Get(..),Binary(..))
import qualified Data.Binary.Put as Put
import qualified Data.Binary.Get as Get


type Type = Word8
type Status = Word8
type Length = Int
type SPID = Word16
type PacketID = Word8
type Window = Word8

data Header = Header !Type !Status !Length !SPID !PacketID !Window


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
  return $ Header pt st (fromIntegral len) spid pcid win


instance Binary Header where
  put = putHeader
  get = getHeader
