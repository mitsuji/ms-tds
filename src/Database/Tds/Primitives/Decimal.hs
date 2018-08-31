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
data Decimal = DecimalS0 !Precision !Fixed0
             | DecimalS1 !Precision !Fixed1
             | DecimalS2 !Precision !Fixed2
             | DecimalS3 !Precision !Fixed3
             | DecimalS4 !Precision !Fixed4
             | DecimalS5 !Precision !Fixed5
             | DecimalS6 !Precision !Fixed6
             | DecimalS7 !Precision !Fixed7
             | DecimalS8 !Precision !Fixed8
             | DecimalS9 !Precision !Fixed9
             | DecimalS10 !Precision !Fixed10
             | DecimalS11 !Precision !Fixed11
             | DecimalS12 !Precision !Fixed12
             | DecimalS13 !Precision !Fixed13
             | DecimalS14 !Precision !Fixed14
             | DecimalS15 !Precision !Fixed15
             | DecimalS16 !Precision !Fixed16
             | DecimalS17 !Precision !Fixed17
             | DecimalS18 !Precision !Fixed18
             | DecimalS19 !Precision !Fixed19
             | DecimalS20 !Precision !Fixed20
             | DecimalS21 !Precision !Fixed21
             | DecimalS22 !Precision !Fixed22
             | DecimalS23 !Precision !Fixed23
             | DecimalS24 !Precision !Fixed24
             | DecimalS25 !Precision !Fixed25
             | DecimalS26 !Precision !Fixed26
             | DecimalS27 !Precision !Fixed27
             | DecimalS28 !Precision !Fixed28
             | DecimalS29 !Precision !Fixed29
             | DecimalS30 !Precision !Fixed30
             | DecimalS31 !Precision !Fixed31
             | DecimalS32 !Precision !Fixed32
             | DecimalS33 !Precision !Fixed33
             | DecimalS34 !Precision !Fixed34
             | DecimalS35 !Precision !Fixed35
             | DecimalS36 !Precision !Fixed36
             | DecimalS37 !Precision !Fixed37
             | DecimalS38 !Precision !Fixed38
             deriving (Show)


bytesToDecimal :: Precision -> Scale -> Word8 -> B.ByteString -> Decimal
bytesToDecimal p s sign bs =
  let
    sign' = if sign == 0x01 then 1 else -1
    i = bytesToInteger bs
  in integerToDecimal p s $ sign' * i


-- [MEMO] signed, little endian
bytesToInteger :: B.ByteString -> Integer
bytesToInteger = B.foldl' f 0 . B.reverse
  where
    f a b = a `shift` 8 .|. fromIntegral b

integerToDecimal :: Precision -> Scale -> Integer -> Decimal
integerToDecimal p s i =
  case s of
    0 -> DecimalS0 p $ MkFixed i 
    1 -> DecimalS1 p $ MkFixed i 
    2 -> DecimalS2 p $ MkFixed i 
    3 -> DecimalS3 p $ MkFixed i 
    4 -> DecimalS4 p $ MkFixed i 
    5 -> DecimalS5 p $ MkFixed i 
    6 -> DecimalS6 p $ MkFixed i 
    7 -> DecimalS7 p $ MkFixed i 
    8 -> DecimalS8 p $ MkFixed i 
    9 -> DecimalS9 p $ MkFixed i 

    10 -> DecimalS10 p $ MkFixed i
    11 -> DecimalS11 p $ MkFixed i
    12 -> DecimalS12 p $ MkFixed i
    13 -> DecimalS13 p $ MkFixed i
    14 -> DecimalS14 p $ MkFixed i
    15 -> DecimalS15 p $ MkFixed i
    16 -> DecimalS16 p $ MkFixed i
    17 -> DecimalS17 p $ MkFixed i
    18 -> DecimalS18 p $ MkFixed i
    19 -> DecimalS19 p $ MkFixed i

    20 -> DecimalS20 p $ MkFixed i
    21 -> DecimalS21 p $ MkFixed i
    22 -> DecimalS22 p $ MkFixed i
    23 -> DecimalS23 p $ MkFixed i
    24 -> DecimalS24 p $ MkFixed i
    25 -> DecimalS25 p $ MkFixed i
    26 -> DecimalS26 p $ MkFixed i
    27 -> DecimalS27 p $ MkFixed i
    28 -> DecimalS28 p $ MkFixed i
    29 -> DecimalS29 p $ MkFixed i

    30 -> DecimalS30 p $ MkFixed i
    31 -> DecimalS31 p $ MkFixed i
    32 -> DecimalS32 p $ MkFixed i
    33 -> DecimalS33 p $ MkFixed i
    34 -> DecimalS34 p $ MkFixed i
    35 -> DecimalS35 p $ MkFixed i
    36 -> DecimalS36 p $ MkFixed i
    37 -> DecimalS37 p $ MkFixed i
    38 -> DecimalS38 p $ MkFixed i

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


decimalToBytes :: Decimal -> (Word8,B.ByteString)
decimalToBytes dec =
  let
    (p,i) = int dec
    sign = if signum i == -1 then 0x00 else 0x01
    bs = integerToBytes (precisionToLen p) $ abs i
  in (sign,bs)
  where
    int :: Decimal -> (Precision,Integer)
    int (DecimalS0  p (MkFixed i)) =  (p,i)
    int (DecimalS1  p (MkFixed i)) =  (p,i)
    int (DecimalS2  p (MkFixed i)) =  (p,i)
    int (DecimalS3  p (MkFixed i)) =  (p,i)
    int (DecimalS4  p (MkFixed i)) =  (p,i)
    int (DecimalS5  p (MkFixed i)) =  (p,i)
    int (DecimalS6  p (MkFixed i)) =  (p,i)
    int (DecimalS7  p (MkFixed i)) =  (p,i)
    int (DecimalS8  p (MkFixed i)) =  (p,i)
    int (DecimalS9  p (MkFixed i)) =  (p,i)
    int (DecimalS10 p (MkFixed i)) =  (p,i)
    int (DecimalS11 p (MkFixed i)) =  (p,i)
    int (DecimalS12 p (MkFixed i)) =  (p,i)
    int (DecimalS13 p (MkFixed i)) =  (p,i)
    int (DecimalS14 p (MkFixed i)) =  (p,i)
    int (DecimalS15 p (MkFixed i)) =  (p,i)
    int (DecimalS16 p (MkFixed i)) =  (p,i)
    int (DecimalS17 p (MkFixed i)) =  (p,i)
    int (DecimalS18 p (MkFixed i)) =  (p,i)
    int (DecimalS19 p (MkFixed i)) =  (p,i)
    int (DecimalS20 p (MkFixed i)) =  (p,i)
    int (DecimalS21 p (MkFixed i)) =  (p,i)
    int (DecimalS22 p (MkFixed i)) =  (p,i)
    int (DecimalS23 p (MkFixed i)) =  (p,i)
    int (DecimalS24 p (MkFixed i)) =  (p,i)
    int (DecimalS25 p (MkFixed i)) =  (p,i)
    int (DecimalS26 p (MkFixed i)) =  (p,i)
    int (DecimalS27 p (MkFixed i)) =  (p,i)
    int (DecimalS28 p (MkFixed i)) =  (p,i)
    int (DecimalS29 p (MkFixed i)) =  (p,i)
    int (DecimalS30 p (MkFixed i)) =  (p,i)
    int (DecimalS31 p (MkFixed i)) =  (p,i)
    int (DecimalS32 p (MkFixed i)) =  (p,i)
    int (DecimalS33 p (MkFixed i)) =  (p,i)
    int (DecimalS34 p (MkFixed i)) =  (p,i)
    int (DecimalS35 p (MkFixed i)) =  (p,i)
    int (DecimalS36 p (MkFixed i)) =  (p,i)
    int (DecimalS37 p (MkFixed i)) =  (p,i)
    int (DecimalS38 p (MkFixed i)) =  (p,i)



