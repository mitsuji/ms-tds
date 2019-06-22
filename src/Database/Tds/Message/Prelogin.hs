{-# OPTIONS_HADDOCK hide #-}
-- PRELOGIN:        https://msdn.microsoft.com/en-us/library/dd357559.aspx

module Database.Tds.Message.Prelogin ( Prelogin (..)
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

import Data.Monoid((<>),mempty)
import Control.Applicative((<$>),(<*>))

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB

import Data.Word (Word8(..),Word16(..),Word32(..),Word64(..))
import Data.Int (Int8(..),Int16(..),Int32(..),Int64(..))

import Data.Binary (Put(..),Get(..),Binary(..))
import qualified Data.Binary.Put as Put
import qualified Data.Binary.Get as Get

import Control.Monad (forM_,foldM_)
import Data.Foldable (maximumBy)




type MajorVer = Word8
type MinorVer = Word8
type BuildVer = Word16
type SubBuildVer = Word16
type Threadid = Word32
type Connid = B.ByteString   -- 16byte GUID
type Activity = B.ByteString -- 16byte GUID
type Sequence = Word32
type Nonce = B.ByteString    -- 32byte NONCE


data PreloginOption = PLOVersion !MajorVer !MinorVer !BuildVer !SubBuildVer
                    | PLOEncription !Word8  -- [TODO] flags
                    | PLOInstopt !B.ByteString
                    | PLOThreadid !(Maybe Threadid)
                    | PLOMars !Word8  -- MARS(Multiple Active Result Sets) supprt -- [TODO] flags
                    | PLOTraceid !Connid !Activity !Sequence
                    | PLOFedAuthRequired !Word8
                    | PLONonceOpt !Nonce
                    deriving (Show)

-- | [\[MS-TDS\] 2.2.6.5 PRELOGIN](https://docs.microsoft.com/en-us/openspecs/windows_protocols/ms-tds/60f56408-0188-4cd5-8b90-25c6f2423868)
newtype Prelogin = Prelogin [PreloginOption]
                 deriving (Show)


preloginOptionsIndexOffset :: [a] -> Int
preloginOptionsIndexOffset ops = (5 * length ops) + 1

preloginOptionPayloadLength :: PreloginOption -> Int
preloginOptionPayloadLength = f
  where
    f (PLOVersion _ _ _ _)   = 1 + 1 + 4
    f (PLOEncription _)      = 1
    f (PLOInstopt io)        = B.length io + 1
    f (PLOThreadid _)        = 4
    f (PLOMars _)            = 1
    f (PLOTraceid _ _ _)     = 16 + 16 + 4 -- [TODO] Test
    f (PLOFedAuthRequired _) = 1           -- [TODO] Test
    f (PLONonceOpt _)        = 32          -- [TODO] Test

-- https://msdn.microsoft.com/en-us/library/dd357559.aspx
putPrelogin :: Prelogin -> Put
putPrelogin (Prelogin ops) = do
  foldM_ putIndex (preloginOptionsIndexOffset ops) ops
  Put.putWord8 0xff -- terminate
  forM_ ops putOpt
  where
    putIndex :: Int -> PreloginOption -> Put.PutM Int
    putIndex offs op = do
      let
        ot = case op of
          PLOVersion _ _ _ _   -> 0x00
          PLOEncription _      -> 0x01
          PLOInstopt _         -> 0x02
          PLOThreadid _        -> 0x03
          PLOMars _            -> 0x04
          PLOTraceid _ _ _     -> 0x05 -- [TODO] Test
          PLOFedAuthRequired _ -> 0x06 -- [TODO] Test
          PLONonceOpt _        -> 0x07 -- [TODO] Test
        len = preloginOptionPayloadLength op
      Put.putWord8 ot
      Put.putWord16be $ fromIntegral offs
      Put.putWord16be $ fromIntegral len
      return $ offs+len
        
    putOpt :: PreloginOption -> Put
    putOpt (PLOVersion ma mi b sb) = do
      Put.putWord8 ma
      Put.putWord8 mi
      Put.putWord16be b
      Put.putWord16be sb
        
    putOpt (PLOEncription enc) = Put.putWord8 enc
        
    putOpt (PLOInstopt io) = do
      Put.putByteString io
      Put.putWord8 0
        
    putOpt (PLOThreadid Nothing) = return ()
    putOpt (PLOThreadid (Just tid)) = Put.putWord32le tid
        
    putOpt (PLOMars mars) = Put.putWord8 mars

    -- [TODO] Test
    putOpt (PLOTraceid ci ac se) = do
      Put.putByteString ci
      Put.putByteString ac
      Put.putWord32le se
    
    -- [TODO] Test
    putOpt (PLOFedAuthRequired b) = Put.putWord8 b

    -- [TODO] Test
    putOpt (PLONonceOpt opt) = Put.putByteString opt



-- https://msdn.microsoft.com/en-us/library/dd340710.aspx
getPrelogin :: Get Prelogin
getPrelogin = do
  idcs <- getIndices
  -- [MEMO] calc totallen from max offset
  let (_,maxoffs,maxoffslen) = maximumBy (\(_,offs1,_) (_,offs2,_) -> compare offs1 offs2) idcs
  let totalLen = maxoffs + maxoffslen
  let offs0 = preloginOptionsIndexOffset idcs
  payl <- Get.getLazyByteString $ fromIntegral $ (fromIntegral totalLen) - offs0

  return $ Prelogin $ flip fmap idcs $ \(ot,offs,len) ->
    Get.runGet (getOpt ot len) $ LB.drop (fromIntegral $ offs - offs0) payl
        
  where
    getIndices :: Get [(Word8,Int,Int)]
    getIndices = do
      ot <- Get.getWord8
      if ot == 0xff
        then return []
        else do index   <- getIndex ot
                indices <- getIndices
                return $ index:indices
        where
          getIndex :: Word8 -> Get (Word8,Int,Int)
          getIndex ot = do
            offs <- fromIntegral <$> Get.getWord16be  -- offset
            len  <- fromIntegral <$> Get.getWord16be  -- len
            return (ot,offs,len)
          
          
                    
    getOpt :: Word8 -> Int -> Get PreloginOption
    
    getOpt 0x00 _ = PLOVersion <$> Get.getWord8
                               <*> Get.getWord8
                               <*> Get.getWord16be
                               <*> Get.getWord16be
                               
    getOpt 0x01 _ = PLOEncription <$> Get.getWord8
    
    getOpt 0x02 len = -- [MEMO] null terminated string
      if len == 1
        then return $ PLOInstopt mempty
        else do bs <- Get.getByteString $ len -1
                return $ PLOInstopt bs
      
    getOpt 0x03 len = do
      if len == 0
        then return $ PLOThreadid Nothing
        else PLOThreadid . Just <$> Get.getWord32le

    getOpt 0x04 _ = PLOMars <$> Get.getWord8
          
    -- [TODO] Test
    getOpt 0x05 _ = PLOTraceid <$> Get.getByteString 16
                               <*> Get.getByteString 16
                               <*> Get.getWord32le

    -- [TODO] Test
    getOpt 0x06 _ = PLOFedAuthRequired <$> Get.getWord8
    
    -- [TODO] Test
    getOpt 0x07 _ = PLONonceOpt <$> Get.getByteString 32
                


instance Binary Prelogin where
  put = putPrelogin
  get = getPrelogin

