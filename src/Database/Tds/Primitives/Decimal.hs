{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
-- https://docs.microsoft.com/en-us/openspecs/windows_protocols/ms-tds/5e02042c-a741-4b5a-b91d-af5e236c5252

module Database.Tds.Primitives.Decimal ( Precision (..)
                                       , Scale (..)
                                       , Decimal (..)
                                       , precisionToLen
                                       , bytesToDecimal
                                       , decimalToBytes
                                       , decimal0,decimal1,decimal2,decimal3,decimal4
                                       , decimal5,decimal6,decimal7,decimal8,decimal9
                                       , decimal10,decimal11,decimal12,decimal13,decimal14
                                       , decimal15,decimal16,decimal17,decimal18,decimal19
                                       , decimal20,decimal21,decimal22,decimal23,decimal24
                                       , decimal25,decimal26,decimal27,decimal28,decimal29
                                       , decimal30,decimal31,decimal32,decimal33,decimal34
                                       , decimal35,decimal36,decimal37,decimal38
                                       ) where

import Data.Monoid ((<>))
import Data.Word (Word8(..))
import Data.Int (Int32(..))
import Data.Fixed (Fixed(..))
import Data.Bits ((.&.),(.|.),shift)
import qualified Data.ByteString as B
import Database.Tds.Primitives.Fixed

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (returnQ)

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


-- data Decimal = DecimalS0 !Fixed0
--             ...
--             | DecimalS38 !Fixed38
--             deriving (Show)
#if MIN_VERSION_template_haskell(2,12,0)
returnQ [
  DataD [] (mkName "Decimal") [] Nothing
  ((flip map) [0..38] $ \i -> NormalC (mkName $ "DecimalS" <> show i) [(Bang NoSourceUnpackedness SourceStrict,ConT (mkName $ "Fixed" <> show i))] )
  [DerivClause Nothing [ConT ''Show]]
  ]
#elif MIN_VERSION_template_haskell(2,11,0)
returnQ [
  DataD [] (mkName "Decimal") [] Nothing
  ((flip map) [0..38] $ \i -> NormalC (mkName $ "DecimalS" <> show i) [(Bang NoSourceUnpackedness SourceStrict,ConT (mkName $ "Fixed" <> show i))] )
  [ConT ''Show]
  ]
#else
returnQ [
  DataD [] (mkName "Decimal") []
  ((flip map) [0..38] $ \i -> NormalC (mkName $ "DecimalS" <> show i) [(IsStrict,ConT (mkName $ "Fixed" <> show i))] )
  [''Show]
  ]
#endif


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
--  case s of
--    0 -> DecimalS0 $ MkFixed i
--    ...
--    38 -> DecimalS38 $ MkFixed i
--    _ -> error "integerToDecimal: invalid scale"
  $(returnQ $ CaseE (VarE 's) $
     (
       (flip map) [0..38] $ \j ->
         Match (LitP $ IntegerL j) (NormalB $ AppE (ConE $ mkName $ "DecimalS" <> show j) $ AppE (ConE $ mkName "MkFixed") (VarE 'i) ) []
     )
     <> [Match WildP (NormalB $ AppE (VarE 'error) (LitE $ StringL "integerToDecimal: invalid scale")) []]
   )



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


--    int :: Decimal -> Integer
--    int (DecimalS0  (MkFixed i)) = i
--    ...
--    int (DecimalS38 (MkFixed i)) = i
returnQ [
  (FunD $ mkName "int")
  $ (flip map) [0..38] $ \j->
      Clause [ConP (mkName $ "DecimalS" <> show j) [ConP (mkName "MkFixed") [VarP $ mkName "i"]]]
      (NormalB $ VarE $ mkName "i")
      []
  ]

decimalToBytes :: Precision -> Decimal -> (Word8,B.ByteString)
decimalToBytes p dec =
  let
    i = int dec
    sign = if signum i == -1 then 0x00 else 0x01
    bs = integerToBytes (precisionToLen p) $ abs i
  in (sign,bs)



-- decimal0 :: Decimal -> Fixed0
-- decimal0 (DecimalS0 f) = f
-- decimal0 _ = error "decimal0: scale mismatch"
returnQ $ (flip map) [0..38] $ \i ->
  (FunD $ mkName $ "decimal" <> show i)
  [
    Clause [ConP (mkName $ "DecimalS" <> show i) [VarP $ mkName "f"]]
    (NormalB $ VarE $ mkName "f")
    []
  ,
    Clause [WildP]
    (NormalB $ AppE (VarE 'error) (LitE $ StringL $ "decimal" <> show i <> ": scale mismatch"))
    []
  ]


