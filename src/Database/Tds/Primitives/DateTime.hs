{-# OPTIONS_HADDOCK hide #-}
-- https://msdn.microsoft.com/en-us/library/ee780895.aspx

module Database.Tds.Primitives.DateTime ( bytesToUtc4
                                        , bytesToUtc8
                                        , utcToBytes4
                                        , utcToBytes8
                                        ) where


import Data.Word (Word16(..),Word32(..))
import Data.Int (Int32(..))

import Data.Time (UTCTime(..))
import Data.Time.Calendar (addDays,diffDays,fromGregorian)

bytesToUtc4 :: Word16 -> Word16 -> UTCTime
bytesToUtc4 wday wmin =
  -- date: the number of days since January 1, 1900.
  -- time: the number of minutes elapsed since 12 AM that day.
  let
    date = addDays (fromIntegral wday) (fromGregorian 1900 1 1)
    time = (fromIntegral wmin) * 60
  in UTCTime date time

bytesToUtc8 :: Int32 -> Word32 -> UTCTime
bytesToUtc8 iday w3hsec =
  -- date: the number of days since January 1, 1900.
  -- time: the number of one three-hundredths of a second (300 counts per second) elapsed since 12 AM that day.
  let
    date = addDays (fromIntegral iday) (fromGregorian 1900 1 1)
    time = (fromIntegral w3hsec) / 300
  in UTCTime date time


utcToBytes4 :: UTCTime -> (Word16,Word16)
utcToBytes4 (UTCTime date time) =
  let
    wday = fromIntegral $ diffDays (fromGregorian 1900 1 1) date
    wmin = truncate $ time / 60
  in (wday,wmin)

utcToBytes8 :: UTCTime -> (Int32,Word32)
utcToBytes8 (UTCTime date time) =
  let
    iday = fromIntegral $ diffDays (fromGregorian 1900 1 1) date
    w3hsec = truncate $ time * 300
  in (iday,w3hsec)

       
