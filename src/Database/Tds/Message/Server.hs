{-# OPTIONS_HADDOCK hide #-}
-- Stream Types:    https://msdn.microsoft.com/en-us/library/dd303435.aspx
-- Data Types:      https://msdn.microsoft.com/en-us/library/dd305325.aspx
-- Data Stream:     https://msdn.microsoft.com/en-us/library/dd340794.aspx
-- Server Messages: https://msdn.microsoft.com/en-us/library/dd357167.aspx


module Database.Tds.Message.Server ( TokenStreams (..)
                                   , TokenStream (..)
                                   
                                   , AltMetaData (..)
                                   
                                   , ColProperty (..)
                                   , CPColNum (..)
                                   , CPTableNum (..)
                                   , CPStatus (..)
                                   , CPColName (..)

                                   , ColMetaData (..)
                                   , MetaColumnData (..)
                                   , MCDUserType (..)
                                   , MCDFlags (..)
                                   , MCDTableName (..)
                                   , MCDColName (..)

                                   , Done (..)
                                   , DoneStatus (..)
                                   , DoneCurCmd (..)
                                   , DoneRowCount (..)

                                   , ECType (..)
                                   , ECNewValue (..)
                                   , ECOldValue (..)

                                   , Info (..)
                                   , InfoNumber (..)
                                   , InfoState (..)
                                   , InfoClass (..)
                                   , InfoMsgText (..)
                                   , InfoServerName (..)
                                   , InfoProcName (..)
                                   , InfoLineNumber (..)

                                   , LAInterface (..)
                                   , LATdsVersion (..)
                                   , LAProgName (..)
                                   , LAProgVersion (..)
                                   
                                   , Offset (..)
                                   , OffsetIdentifier (..)
                                   , OffsetLength (..)
                                   
                                   , ReturnValue (..)
                                   , RVParamOrdinal (..)
                                   , RVParamName (..)
                                   , RVStatus (..)
                                   , RVUserType (..)
                                   , RVFlags (..)
                                   
                                   , RowColumnData (..)
                                   , TextPointer (..)
                                   , TimeStamp (..)

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

import Control.Monad.State (StateT(..),evalStateT,put,get,modify)
import Control.Monad.Trans (lift)

import Database.Tds.Message.Prelogin
import Database.Tds.Message.DataStream



type MCDUserType = Word16
type MCDFlags = Word16
type MCDTableName = T.Text
type MCDColName = T.Text

data MetaColumnData = MetaColumnData !MCDUserType !MCDFlags !TypeInfo !(Maybe MCDTableName) !MCDColName
                    deriving (Show)


type RVParamOrdinal = Word16
type RVParamName = T.Text
type RVStatus = Word8
type RVUserType = Word16 -- [MEMO] TDS 7.2 -> Word32
type RVFlags = Word16

data ReturnValue = ReturnValue !RVParamOrdinal !RVParamName !RVStatus !RVUserType !RVFlags !TypeInfo !RawBytes
                 deriving (Show)


-- [MEMO] not newtype for (TDS 7.4 CekTable)
data ColMetaData = ColMetaData ![MetaColumnData]
                 deriving (Show)


-- [TODO] implement data type
data AltMetaData = AltMetaData
                 deriving (Show)

-- [TODO] implement data type
data AltRowData = AltRowData
                deriving (Show)


type OffsetIdentifier = Word16
type OffsetLength = Word16

data Offset = Offset !OffsetIdentifier !OffsetLength
            deriving (Show)


data MetaData = MetaData !(Maybe ColMetaData) !(Maybe AltMetaData)
              deriving (Show)



type TextPointer = B.ByteString
type TimeStamp = Word64

data RowColumnData = RCDOrdinal !RawBytes
                   | RCDLarge !(Maybe TextPointer) !(Maybe TimeStamp) !RawBytes
                   deriving (Show)


type CPColNum = Word8
type CPTableNum = Word8
type CPStatus = Word8
type CPColName = T.Text
data ColProperty = ColProperty !CPColNum !CPTableNum !CPStatus !(Maybe CPColName)
                 deriving (Show)

type DoneStatus = Word16
type DoneCurCmd = Word16
type DoneRowCount = Int32 -- [MEMO] TDS 7.2 -> Word64
data Done = Done !DoneStatus !DoneCurCmd !DoneRowCount
          deriving (Show)

type ECType = Word8 -- [TODO] To be detailed
type ECNewValue = B.ByteString
type ECOldValue = B.ByteString

type InfoNumber = Int32
type InfoState = Word8
type InfoClass = Word8
type InfoMsgText = T.Text
type InfoServerName = T.Text
type InfoProcName = T.Text
type InfoLineNumber = Word16 -- [MEMO] TDS 7.2 -> (error:Int32,info:Word32)
data Info = Info !InfoNumber !InfoState !InfoClass !InfoMsgText !InfoServerName !InfoProcName !InfoLineNumber
          deriving (Show)

type LAInterface = Word8
type LATdsVersion = Word32
type LAProgName = T.Text
type LAProgVersion = Word32 -- [TODO] split bytes

data TokenStream = TSAltMetaData !AltMetaData

                 | TSAltRow !AltRowData
                 
                 | TSColInfo ![ColProperty]
                   
                 | TSColMetaData !(Maybe ColMetaData)

                 | TSDone !Done

                 | TSDoneInProc !Done

                 | TSDoneProc !Done

                 | TSEnvChange !ECType !ECNewValue !ECOldValue

                 | TSError !Info

                 | TSInfo !Info

                 | TSLoginAck !LAInterface !LATdsVersion !LAProgName !LAProgVersion

                 | TSOffset !Offset
                   
                 | TSOrder ![Word16]
                   
                 | TSReturnStatus !Int32
                   
                 | TSReturnValue !ReturnValue
                   
                 | TSRow ![RowColumnData]
                 
                 | TSSSPI !B.ByteString
                 
                 | TSTabName ![[T.Text]]
                   
                 deriving (Show)


getTokenStreamS :: StateT MetaData Get TokenStream
getTokenStreamS = do
  pt <- lift Get.getWord8
  case pt of
    0x88 -> getAltMetaDataS
    0xd3 -> getAltRowS
    0xa5 -> lift getColInfo
    0x81 -> getColMetaDataS
    0xfd -> lift getDone
    0xff -> lift getDoneInProc
    0xfe -> lift getDoneProc
    0xe3 -> lift getEnvChange
    0xaa -> lift getError
    0xab -> lift getInfo
    0xad -> lift getLoginAck
    0x78 -> lift getOffset
    0xa9 -> lift getOrder
    0x79 -> lift getReturnStatus
    0xac -> lift getReturnValue
    0xd1 -> getRowS
    0xed -> lift getSSPI
    0xa4 -> lift getTabName
    _ -> lift $ getOther pt
  where

     -- [TODO] find SQL statement that generates this type of packet, implementation
    getAltMetaDataS :: StateT MetaData Get TokenStream
    getAltMetaDataS =
      fail "getTokenStreamS.getAltMetaDataS: packet type ALTMEATADA not supportd"
--      return $ TSAltMetaData AltMetaData

    -- [TODO] find SQL statement that generates this type of packet, implementation
    getAltRowS :: StateT MetaData Get TokenStream
    getAltRowS =
      fail "getTokenStreamS.getAltRowS: packet type ALTROW not supportd"
--      return $ TSAltRow AltRowData
    
    getColInfo :: Get TokenStream
    getColInfo = do
      len <- fromIntegral <$> Get.getWord16le
      bs  <- Get.getLazyByteString len
      return $ TSColInfo $ Get.runGet (getColProperties len) bs
      where
        getColProperties :: Int64 -> Get [ColProperty]
        getColProperties len = f
          where
            f :: Get [ColProperty]
            f = do
              br <- Get.bytesRead
              if br >= len
                then return []
                else do x  <- getColProperty
                        xs <- f
                        return $ x:xs
            
        getColProperty :: Get ColProperty
        getColProperty = do
          colNum   <- Get.getWord8
          tableNum <- Get.getWord8
          status   <- Get.getWord8
          colName  <- if (status .&. 0x20 /= 0x00) -- [MEMO] DIFFERENT_NAME
                      then Just <$> getText8
                      else return Nothing
          return $ ColProperty colNum tableNum status colName
      
    
    getColMetaDataS :: StateT MetaData Get TokenStream
    getColMetaDataS = do
      cols <- lift Get.getWord16le
      if cols == 0xffff
        then return $ TSColMetaData Nothing
        else do cmd <- lift $ Just . ColMetaData <$> getColumnDatas (fromIntegral cols) 0
                modify $ \(MetaData _ mamd) -> (MetaData cmd mamd)
                return $ TSColMetaData cmd
        where
          getColumnDatas :: Int -> Int -> Get [MetaColumnData]
          getColumnDatas max cnt =
            if cnt >= max
              then return []
              else do x  <- getColumnData
                      xs <- getColumnDatas max (cnt+1)
                      return $ x:xs
                
          getColumnData :: Get MetaColumnData
          getColumnData = do
            userType <- Get.getWord16le
            flags <- Get.getWord16le
            typeInfo <- Data.Binary.get
            maybeTableName <- case typeInfo of
              TIText{}  -> Just <$> getText16
              TINText{} -> Just <$> getText16
              TIImage{} -> Just <$> getText16
              _ -> return Nothing
            col <- getText8
            
            return $ MetaColumnData userType flags typeInfo maybeTableName col



    getDone :: Get TokenStream
    getDone = do
      status <- Get.getWord16le
      curCmd <- Get.getWord16le
      doneRowCount <- Get.getInt32le -- [MEMO] TDS 7.2 -> Word64
--      doneRowCount <- Get.getWord64le
      return $ TSDone $ Done status curCmd doneRowCount
          
    getDoneInProc :: Get TokenStream
    getDoneInProc = do
      status <- Get.getWord16le
      curCmd <- Get.getWord16le
      doneRowCount <- Get.getInt32le -- [MEMO] TDS 7.2 -> Word64
--      doneRowCount <- Get.getWord64le
      return $ TSDoneInProc $ Done status curCmd doneRowCount
          
    getDoneProc :: Get TokenStream
    getDoneProc = do
      status <- Get.getWord16le
      curCmd <- Get.getWord16le
      doneRowCount <- Get.getInt32le -- [MEMO] TDS 7.2 -> Word64
--      doneRowCount <- Get.getWord64le
      return $ TSDoneProc $ Done status curCmd doneRowCount
          

    getEnvChange :: Get TokenStream
    getEnvChange = do
      slen  <- Get.getWord16le
      envCode <- Get.getWord8
      -- [TODO] to be detailed types
      (old,new) <- case envCode of
        0x07 -> do -- [MEMO] report SQL Collation
          oldLen <- Get.getWord8
          old <- getByteString oldLen
          newLen <- Get.getWord8
          new <- getByteString newLen
          return (old,new)
        _ -> do
          oldLen <- Get.getWord8
          old <- getByteString $ oldLen * 2
          newLen <- Get.getWord8
          new <- getByteString $ newLen * 2
          return (old,new)
      return $ TSEnvChange envCode old new


    
    getError :: Get TokenStream
    getError = do
      slen  <- Get.getWord16le
      number <- Get.getInt32le
      state <- Get.getWord8
      mclass <- Get.getWord8
      message <- getText16
      server <- getText8
      process <- getText8

      line <- Get.getWord16le -- [MEMO] TDS 7.2 -> Int32
--      line <- Get.getInt32le
          
      return $ TSError $ Info number state mclass message server process line


    getInfo :: Get TokenStream
    getInfo = do
      slen  <- Get.getWord16le
      number <- Get.getInt32le
      state <- Get.getWord8
      mclass <- Get.getWord8
      message <- getText16
      server <- getText8
      process <- getText8

      line <- Get.getWord16le -- [MEMO] TDS 7.2 -> Word32
--      line <- Get.getWord32le
          
      return $ TSInfo $ Info number state mclass message server process line


    getLoginAck :: Get TokenStream
    getLoginAck = do
      slen  <- Get.getWord16le
      interface <- Get.getWord8
      tdsVer <- Get.getWord32be
      serverLen <- Get.getWord8
      bserver <- getByteString $ serverLen * 2
      let
        bserver' = B.take (B.length bserver -4) bserver
        server   = T.decodeUtf16LE bserver'
      servVer <- Get.getWord32be
      return $ TSLoginAck interface tdsVer server servVer


    -- [TODO] find SQL statement that generates this type of packet, test
    getOffset :: Get TokenStream
    getOffset = do
      ofs <- Offset <$> Get.getWord16le <*> Get.getWord16le
      return $ TSOffset ofs

    getOrder :: Get TokenStream
    getOrder = do
      len <- Get.getWord16le
      nums <- mapM (\_ -> Get.getWord16le) [1..(div len  2)]
      return $ TSOrder nums

    getReturnStatus :: Get TokenStream
    getReturnStatus = do
      val <- Get.getInt32le
      return $ TSReturnStatus val

    getReturnValue :: Get TokenStream
    getReturnValue = do
      po <- Get.getWord16le
      pn <- getText8
      st <- Get.getWord8
      ut <- Get.getWord16le -- [MEMO] TDS 7.2 -> Word32
--      ut <- Get.getWord32le
      fl <- Get.getWord16le
      ti <- Data.Binary.get
      vl <- getRawBytes ti
      return $ TSReturnValue $ ReturnValue po pn st ut fl ti vl


    getRowS :: StateT MetaData Get TokenStream
    getRowS = do
      -- [TODO] raise error when Nothing
      Just (ColMetaData colDatas) <- (\(MetaData mcmd mamd) -> mcmd) <$> Control.Monad.State.get
      datas <- lift $ mapM (getColumnData . (\(MetaColumnData _ _ ti _ _) -> ti)) colDatas
      return $ TSRow datas
        where
          getColumnData :: TypeInfo -> Get RowColumnData
          getColumnData ti = do
            case ti of
              TIText{}  -> getCDLarge ti
              TINText{} -> getCDLarge ti
              TIImage{} -> getCDLarge ti
              _ -> RCDOrdinal <$> getRawBytes ti

          getCDLarge :: TypeInfo -> Get RowColumnData
          getCDLarge ti = do
            len <- Get.getWord8
            if len == 0
              then do
                -- [MEMO] should read 32bit ?
                case ti of
                  TIText{}  -> return $ RCDLarge Nothing Nothing Nothing
                  TINText{} -> return $ RCDLarge Nothing Nothing Nothing
                  TIImage{} -> return $ RCDLarge Nothing Nothing Nothing
              else do tp <- getByteString len
                      ts <- Get.getWord64le
                      dt <- getRawBytes ti
                      return $ RCDLarge (Just tp) (Just ts) dt


    -- [TODO] find SQL statement that generates this type of packet, test
    getSSPI :: Get TokenStream
    getSSPI = do
      len <- Get.getWord16le
      bs <- getByteString len
      return $ TSSSPI bs
    
    getTabName :: Get TokenStream
    getTabName = do
      len <- fromIntegral <$> Get.getWord16le
      bs  <- Get.getLazyByteString len
      return $ TSTabName $ Get.runGet (getAllTableNames len) bs
      where
        getAllTableNames :: Int64 -> Get [[T.Text]]
        getAllTableNames len = f
          where
            f :: Get [[T.Text]]
            f = do
              br <- Get.bytesRead
              if br >= len
                then return []
                else do x <- getTableName
                        xs <-f
                        return $ x:xs

        getTableName :: Get [T.Text]
        getTableName = do
          numParts <- fromIntegral <$> Get.getWord8
          names <- mapM (\_ -> getText16 ) [1..numParts]
          return names
          
          
          

      
      
    getOther :: Word8 -> Get TokenStream
    getOther pt = do
      case pt of
        0xae ->
          -- FEATUREEXTACK
          -- [MEMO] introduced in TDS 7.4
          fail "getTokenStreamS.getOther: packet type FEATUREEXTACK not supportd"
        0xee ->
          -- FEDAUTHINFO
          -- [MEMO] introduced in TDS 7.4
          fail "getTokenStreamS.getOther: packet type FEDAUTHINFO not supportd"
        0xd2 ->
          -- NBCROW
          -- [MEMO] introduced in TDS 7.3.B
          fail "getTokenStreamS.getOther: packet type NBCROW not supported"
        0xe4 ->
          -- SESSIONSTATE
          -- [MEMO] introduced in TDS 7.4
          fail "getTokenStreamS.getOther: packet type SESSIONSTATE not supportd"
        0x01 ->
          -- TVP ROW
          -- https://msdn.microsoft.com/en-us/library/dd304813.aspx
          -- [MEMO] not here ?
          -- [MEMO] introduced in TDS 7.3 ?
          fail "getTokenStreamS.getOther: packet type TVP ROW not supported"
        _ -> fail "getTokenStreamS.getOther: invalid packet type"


          



getTokenStreamsS :: StateT MetaData Get [TokenStream]
getTokenStreamsS = f
  where
    f :: StateT MetaData Get [TokenStream]
    f = do
      x <- getTokenStreamS
      if final x
        then return $ x : []
        else do xs <- f
                return $ x : xs

    final :: TokenStream -> Bool
    final (TSDone (Done st _ _))       = not $ containsMoreBit st
    final (TSDoneInProc (Done st _ _)) = not $ containsMoreBit st
    final (TSDoneProc (Done st _ _))   = not $ containsMoreBit st
    final _ = False

    containsMoreBit :: Word16 -> Bool
    containsMoreBit st = st .&. 0x01 == 0x01 -- [MEMO] 0x1 more bit



newtype TokenStreams = TokenStreams [TokenStream]
                     deriving (Show)



getTokenStreams :: Get TokenStreams
getTokenStreams = do
  rs <- (evalStateT getTokenStreamsS) (MetaData Nothing Nothing)
  return $ TokenStreams rs


putTokenStreams :: TokenStreams -> Put
putTokenStreams = undefined -- [TODO] implement put function



instance Binary TokenStreams where
  put = putTokenStreams
  get = getTokenStreams




getByteString :: Integral a => a -> Get B.ByteString
getByteString len = Get.getByteString $ fromIntegral len

getText :: Integral a => a -> Get T.Text
getText len = T.decodeUtf16LE <$> getByteString len

getText8 :: Get T.Text
getText8 = Get.getWord8 >>= \len -> getText $ len * 2

getText16 :: Get T.Text
getText16 = Get.getWord16le >>= \len -> getText $ len * 2

