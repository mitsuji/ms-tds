{-# OPTIONS_HADDOCK hide #-}
-- https://docs.microsoft.com/en-us/openspecs/windows_protocols/ms-tds/3d29e8dc-218a-42c6-9ba4-947ebca9fd7e

module Database.Tds.Primitives.Collation ( Collation32 (..)
                                         , SortId (..)
                                         , Collation (..)
                                         , getCollation
                                         , putCollation
                                         ) where

import Data.Word (Word8(..),Word32(..))

import Data.Binary (Put(..),Get(..))
import qualified Data.Binary.Put as Put
import qualified Data.Binary.Get as Get


type Collation32 = Word32
type SortId = Word8

-- | [\[MS-TDS\] 2.2.5.1.2 Collation Rule Definition](https://docs.microsoft.com/en-us/openspecs/windows_protocols/ms-tds/3d29e8dc-218a-42c6-9ba4-947ebca9fd7e)
data Collation = Collation !Collation32 !SortId
               deriving (Show)


getCollation :: Get Collation
getCollation = do
  coll <- Get.getWord32be
  sort <- Get.getWord8
  return $ Collation coll sort

putCollation :: Collation -> Put
putCollation (Collation coll sort) = do
  Put.putWord32be coll
  Put.putWord8 sort

