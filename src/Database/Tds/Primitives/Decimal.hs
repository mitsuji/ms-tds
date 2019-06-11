{-# OPTIONS_HADDOCK hide #-}
-- https://docs.microsoft.com/en-us/openspecs/windows_protocols/ms-tds/5e02042c-a741-4b5a-b91d-af5e236c5252

module Database.Tds.Primitives.Decimal ( Precision (..)
                                       , Scale (..)
                                       , Decimal (..)
                                       , precisionToLen
                                       , bytesToDecimal
                                       , decimalToBytes
                                       ) where

import Data.Word (Word8(..))
import Data.Int (Int32(..))
import Data.Fixed (Fixed(..))
import Data.Bits ((.&.),(.|.),shift)
import qualified Data.ByteString as B
import Database.Tds.Primitives.Fixed

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


-- [MEMO] Correctness is not sure...
-- [TODO] Test
data Decimal = DecimalS0 !Fixed0
             | DecimalS1 !Fixed1
             | DecimalS2 !Fixed2
             | DecimalS3 !Fixed3
             | DecimalS4 !Fixed4
             | DecimalS5 !Fixed5
             | DecimalS6 !Fixed6
             | DecimalS7 !Fixed7
             | DecimalS8 !Fixed8
             | DecimalS9 !Fixed9
             | DecimalS10 !Fixed10
             | DecimalS11 !Fixed11
             | DecimalS12 !Fixed12
             | DecimalS13 !Fixed13
             | DecimalS14 !Fixed14
             | DecimalS15 !Fixed15
             | DecimalS16 !Fixed16
             | DecimalS17 !Fixed17
             | DecimalS18 !Fixed18
             | DecimalS19 !Fixed19
             | DecimalS20 !Fixed20
             | DecimalS21 !Fixed21
             | DecimalS22 !Fixed22
             | DecimalS23 !Fixed23
             | DecimalS24 !Fixed24
             | DecimalS25 !Fixed25
             | DecimalS26 !Fixed26
             | DecimalS27 !Fixed27
             | DecimalS28 !Fixed28
             | DecimalS29 !Fixed29
             | DecimalS30 !Fixed30
             | DecimalS31 !Fixed31
             | DecimalS32 !Fixed32
             | DecimalS33 !Fixed33
             | DecimalS34 !Fixed34
             | DecimalS35 !Fixed35
             | DecimalS36 !Fixed36
             | DecimalS37 !Fixed37
             | DecimalS38 !Fixed38
             deriving (Show)


bytesToDecimal :: Scale -> Word8 -> B.ByteString -> Decimal
bytesToDecimal s sign bs =
  let
    sign' = if sign == 0x01 then 1 else -1
    i = bytesToInteger bs
  in integerToDecimal s $ sign' * i


-- [MEMO] signed, little endian
bytesToInteger :: B.ByteString -> Integer
bytesToInteger = B.foldl' f 0 . B.reverse
  where
    f a b = a `shift` 8 .|. fromIntegral b

integerToDecimal :: Scale -> Integer -> Decimal
integerToDecimal s i =
  case s of
    0 -> DecimalS0 $ MkFixed i 
    1 -> DecimalS1 $ MkFixed i 
    2 -> DecimalS2 $ MkFixed i 
    3 -> DecimalS3 $ MkFixed i 
    4 -> DecimalS4 $ MkFixed i 
    5 -> DecimalS5 $ MkFixed i 
    6 -> DecimalS6 $ MkFixed i 
    7 -> DecimalS7 $ MkFixed i 
    8 -> DecimalS8 $ MkFixed i 
    9 -> DecimalS9 $ MkFixed i 

    10 -> DecimalS10 $ MkFixed i
    11 -> DecimalS11 $ MkFixed i
    12 -> DecimalS12 $ MkFixed i
    13 -> DecimalS13 $ MkFixed i
    14 -> DecimalS14 $ MkFixed i
    15 -> DecimalS15 $ MkFixed i
    16 -> DecimalS16 $ MkFixed i
    17 -> DecimalS17 $ MkFixed i
    18 -> DecimalS18 $ MkFixed i
    19 -> DecimalS19 $ MkFixed i

    20 -> DecimalS20 $ MkFixed i
    21 -> DecimalS21 $ MkFixed i
    22 -> DecimalS22 $ MkFixed i
    23 -> DecimalS23 $ MkFixed i
    24 -> DecimalS24 $ MkFixed i
    25 -> DecimalS25 $ MkFixed i
    26 -> DecimalS26 $ MkFixed i
    27 -> DecimalS27 $ MkFixed i
    28 -> DecimalS28 $ MkFixed i
    29 -> DecimalS29 $ MkFixed i

    30 -> DecimalS30 $ MkFixed i
    31 -> DecimalS31 $ MkFixed i
    32 -> DecimalS32 $ MkFixed i
    33 -> DecimalS33 $ MkFixed i
    34 -> DecimalS34 $ MkFixed i
    35 -> DecimalS35 $ MkFixed i
    36 -> DecimalS36 $ MkFixed i
    37 -> DecimalS37 $ MkFixed i
    38 -> DecimalS38 $ MkFixed i

    _ -> error "integerToDecimal: invalid scale"


-- [MEMO] signed, little endian
integerToBytes :: Word8 -> Integer -> B.ByteString
integerToBytes len i = B.pack $ f len i
  where
    f :: Word8 -> Integer -> [Word8]
    f 0 _ = []
    f len i =
      let
        (d,m) = divMod i 0xff
      in (fromIntegral m) : f (len-1) d


decimalToBytes :: Precision -> Decimal -> (Word8,B.ByteString)
decimalToBytes p dec =
  let
    i = int dec
    sign = if signum i == -1 then 0x00 else 0x01
    bs = integerToBytes (precisionToLen p) $ abs i
  in (sign,bs)
  where
    int :: Decimal -> Integer
    int (DecimalS0  (MkFixed i)) = i
    int (DecimalS1  (MkFixed i)) = i
    int (DecimalS2  (MkFixed i)) = i
    int (DecimalS3  (MkFixed i)) = i
    int (DecimalS4  (MkFixed i)) = i
    int (DecimalS5  (MkFixed i)) = i
    int (DecimalS6  (MkFixed i)) = i
    int (DecimalS7  (MkFixed i)) = i
    int (DecimalS8  (MkFixed i)) = i
    int (DecimalS9  (MkFixed i)) = i
    int (DecimalS10 (MkFixed i)) = i
    int (DecimalS11 (MkFixed i)) = i
    int (DecimalS12 (MkFixed i)) = i
    int (DecimalS13 (MkFixed i)) = i
    int (DecimalS14 (MkFixed i)) = i
    int (DecimalS15 (MkFixed i)) = i
    int (DecimalS16 (MkFixed i)) = i
    int (DecimalS17 (MkFixed i)) = i
    int (DecimalS18 (MkFixed i)) = i
    int (DecimalS19 (MkFixed i)) = i
    int (DecimalS20 (MkFixed i)) = i
    int (DecimalS21 (MkFixed i)) = i
    int (DecimalS22 (MkFixed i)) = i
    int (DecimalS23 (MkFixed i)) = i
    int (DecimalS24 (MkFixed i)) = i
    int (DecimalS25 (MkFixed i)) = i
    int (DecimalS26 (MkFixed i)) = i
    int (DecimalS27 (MkFixed i)) = i
    int (DecimalS28 (MkFixed i)) = i
    int (DecimalS29 (MkFixed i)) = i
    int (DecimalS30 (MkFixed i)) = i
    int (DecimalS31 (MkFixed i)) = i
    int (DecimalS32 (MkFixed i)) = i
    int (DecimalS33 (MkFixed i)) = i
    int (DecimalS34 (MkFixed i)) = i
    int (DecimalS35 (MkFixed i)) = i
    int (DecimalS36 (MkFixed i)) = i
    int (DecimalS37 (MkFixed i)) = i
    int (DecimalS38 (MkFixed i)) = i



