{-# OPTIONS_HADDOCK hide #-}
-- https://docs.microsoft.com/en-us/openspecs/windows_protocols/ms-tds/1266679d-cd6e-492a-b2b2-3a9ba004196d

module Database.Tds.Primitives.Money ( Money (..)
                                     , moneyToBytes4
                                     , moneyToBytes8
                                     , bytesToMoney4
                                     , bytesToMoney8
                                     ) where

import Data.Int (Int32(..))
import Data.Fixed (Fixed(..))
import Data.Bits ((.&.),shift)
import Database.Tds.Primitives.Fixed

type Money = Fixed4

bytesToMoney4 :: Int32 -> Money
bytesToMoney4 i = MkFixed $ fromIntegral i

bytesToMoney8 :: Int32 -> Int32 -> Money
bytesToMoney8 m l =
  let
    m' = fromIntegral m
    l' = fromIntegral l
  in MkFixed $ (shift m' 32) + l'


moneyToBytes4 :: Money -> Int32
moneyToBytes4 (MkFixed i) = fromIntegral i

moneyToBytes8 :: Money -> (Int32,Int32)
moneyToBytes8 (MkFixed i) =
  let
    m = shift i (-32)
    l = i .&. 0xffffffff
  in (fromIntegral m, fromIntegral l)

