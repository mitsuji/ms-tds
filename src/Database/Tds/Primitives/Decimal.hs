{-# OPTIONS_HADDOCK hide #-}
-- https://docs.microsoft.com/en-us/openspecs/windows_protocols/ms-tds/5e02042c-a741-4b5a-b91d-af5e236c5252

module Database.Tds.Primitives.Decimal ( Precision (..)
                                       , Scale (..)
                                       , precisionToLen
                                       , bytesToFixed
                                       , fixedToBytes
                                       ) where

import Data.Monoid ((<>))
import Data.Word (Word8(..))
import Data.Fixed (Fixed(..),HasResolution(..))
import Data.Bits ((.|.),shift)
import qualified Data.ByteString as B

type Precision = Word8
type Scale = Word8


precisionToLen :: Precision -> Word8
precisionToLen p =
  case p of
    _ | 1  <= p && p <= 9  -> 4
    _ | 10 <= p && p <= 19 -> 8
    _ | 20 <= p && p <= 28 -> 12
    _ | 29 <= p && p <= 38 -> 16
    _ -> error "precisionToLen: invalid Precision"



-- [MEMO] signed, little endian
bytesToInteger :: B.ByteString -> Integer
bytesToInteger = B.foldl' f 0 . B.reverse
  where
    f a b = a `shift` 8 .|. fromIntegral b

bytesToFixed :: (HasResolution a) => Word8 -> B.ByteString -> Fixed a
bytesToFixed sign bs =
  let
    sign' = if sign == 0x01 then 1 else -1
    i = bytesToInteger bs
  in MkFixed $ sign' * i



-- [MEMO] signed, little endian
integerToBytes :: Word8 -> Integer -> B.ByteString
integerToBytes len i = B.pack $ f len i
  where
    f :: Word8 -> Integer -> [Word8]
    f 0 _ = []
    f len i =
      let
        (d,m) = divMod i 0x100
      in (fromIntegral m) : f (len-1) d


fixedToBytes :: (HasResolution a) => Precision -> Fixed a -> (Word8,B.ByteString)
fixedToBytes p (MkFixed i) =
  let
    sign = if signum i == -1 then 0x00 else 0x01
    bs = integerToBytes (precisionToLen p) $ abs i
  in (sign,bs)


