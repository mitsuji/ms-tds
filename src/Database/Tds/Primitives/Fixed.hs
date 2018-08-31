module Database.Tds.Primitives.Fixed ( Fixed0 (..)
                                     , Fixed1 (..)
                                     , Fixed2 (..)
                                     , Fixed3 (..)
                                     , Fixed4 (..)
                                     , Fixed5 (..)
                                     , Fixed6 (..)
                                     , Fixed7 (..)
                                     , Fixed8 (..)
                                     , Fixed9 (..)
                                     , Fixed10 (..)
                                     , Fixed11 (..)
                                     , Fixed12 (..)
                                     , Fixed13 (..)
                                     , Fixed14 (..)
                                     , Fixed15 (..)
                                     , Fixed16 (..)
                                     , Fixed17 (..)
                                     , Fixed18 (..)
                                     , Fixed19 (..)
                                     , Fixed20 (..)
                                     , Fixed21 (..)
                                     , Fixed22 (..)
                                     , Fixed23 (..)
                                     , Fixed24 (..)
                                     , Fixed25 (..)
                                     , Fixed26 (..)
                                     , Fixed27 (..)
                                     , Fixed28 (..)
                                     , Fixed29 (..)
                                     , Fixed30 (..)
                                     , Fixed31 (..)
                                     , Fixed32 (..)
                                     , Fixed33 (..)
                                     , Fixed34 (..)
                                     , Fixed35 (..)
                                     , Fixed36 (..)
                                     , Fixed37 (..)
                                     , Fixed38 (..)
                                     , Exp0 (..)
                                     , Exp1 (..)
                                     , Exp2 (..)
                                     , Exp3 (..)
                                     , Exp4 (..)
                                     , Exp5 (..)
                                     , Exp6 (..)
                                     , Exp7 (..)
                                     , Exp8 (..)
                                     , Exp9 (..)
                                     , Exp10 (..)
                                     , Exp11 (..)
                                     , Exp12 (..)
                                     , Exp13 (..)
                                     , Exp14 (..)
                                     , Exp15 (..)
                                     , Exp16 (..)
                                     , Exp17 (..)
                                     , Exp18 (..)
                                     , Exp19 (..)
                                     , Exp20 (..)
                                     , Exp21 (..)
                                     , Exp22 (..)
                                     , Exp23 (..)
                                     , Exp24 (..)
                                     , Exp25 (..)
                                     , Exp26 (..)
                                     , Exp27 (..)
                                     , Exp28 (..)
                                     , Exp29 (..)
                                     , Exp30 (..)
                                     , Exp31 (..)
                                     , Exp32 (..)
                                     , Exp33 (..)
                                     , Exp34 (..)
                                     , Exp35 (..)
                                     , Exp36 (..)
                                     , Exp37 (..)
                                     , Exp38 (..)
                                     ) where


import Data.Fixed (HasResolution(..),Fixed(..))

data Exp0
data Exp1
data Exp2
data Exp3
data Exp4
data Exp5
data Exp6
data Exp7
data Exp8
data Exp9

data Exp10
data Exp11
data Exp12
data Exp13
data Exp14
data Exp15
data Exp16
data Exp17
data Exp18
data Exp19

data Exp20
data Exp21
data Exp22
data Exp23
data Exp24
data Exp25
data Exp26
data Exp27
data Exp28
data Exp29

data Exp30
data Exp31
data Exp32
data Exp33
data Exp34
data Exp35
data Exp36
data Exp37
data Exp38

instance HasResolution Exp0 where resolution _  = 1
instance HasResolution Exp1 where resolution _  = 10
instance HasResolution Exp2 where resolution _  = 100
instance HasResolution Exp3 where resolution _  = 1000
instance HasResolution Exp4 where resolution _  = 10000
instance HasResolution Exp5 where resolution _  = 100000
instance HasResolution Exp6 where resolution _  = 1000000
instance HasResolution Exp7 where resolution _  = 10000000
instance HasResolution Exp8 where resolution _  = 100000000
instance HasResolution Exp9 where resolution _  = 1000000000

instance HasResolution Exp10 where resolution _ = 10000000000
instance HasResolution Exp11 where resolution _ = 100000000000
instance HasResolution Exp12 where resolution _ = 1000000000000
instance HasResolution Exp13 where resolution _ = 10000000000000
instance HasResolution Exp14 where resolution _ = 100000000000000
instance HasResolution Exp15 where resolution _ = 1000000000000000
instance HasResolution Exp16 where resolution _ = 10000000000000000
instance HasResolution Exp17 where resolution _ = 100000000000000000
instance HasResolution Exp18 where resolution _ = 1000000000000000000
instance HasResolution Exp19 where resolution _ = 10000000000000000000

instance HasResolution Exp20 where resolution _ = 100000000000000000000
instance HasResolution Exp21 where resolution _ = 1000000000000000000000
instance HasResolution Exp22 where resolution _ = 10000000000000000000000
instance HasResolution Exp23 where resolution _ = 100000000000000000000000
instance HasResolution Exp24 where resolution _ = 1000000000000000000000000
instance HasResolution Exp25 where resolution _ = 10000000000000000000000000
instance HasResolution Exp26 where resolution _ = 100000000000000000000000000
instance HasResolution Exp27 where resolution _ = 1000000000000000000000000000
instance HasResolution Exp28 where resolution _ = 10000000000000000000000000000
instance HasResolution Exp29 where resolution _ = 100000000000000000000000000000

instance HasResolution Exp30 where resolution _ = 1000000000000000000000000000000
instance HasResolution Exp31 where resolution _ = 10000000000000000000000000000000
instance HasResolution Exp32 where resolution _ = 100000000000000000000000000000000
instance HasResolution Exp33 where resolution _ = 1000000000000000000000000000000000
instance HasResolution Exp34 where resolution _ = 10000000000000000000000000000000000
instance HasResolution Exp35 where resolution _ = 100000000000000000000000000000000000
instance HasResolution Exp36 where resolution _ = 1000000000000000000000000000000000000
instance HasResolution Exp37 where resolution _ = 10000000000000000000000000000000000000
instance HasResolution Exp38 where resolution _ = 100000000000000000000000000000000000000

type Fixed0 = Fixed Exp0
type Fixed1 = Fixed Exp1
type Fixed2 = Fixed Exp2
type Fixed3 = Fixed Exp3
type Fixed4 = Fixed Exp4
type Fixed5 = Fixed Exp5
type Fixed6 = Fixed Exp6
type Fixed7 = Fixed Exp7
type Fixed8 = Fixed Exp8
type Fixed9 = Fixed Exp9

type Fixed10 = Fixed Exp10
type Fixed11 = Fixed Exp11
type Fixed12 = Fixed Exp12
type Fixed13 = Fixed Exp13
type Fixed14 = Fixed Exp14
type Fixed15 = Fixed Exp15
type Fixed16 = Fixed Exp16
type Fixed17 = Fixed Exp17
type Fixed18 = Fixed Exp18
type Fixed19 = Fixed Exp19

type Fixed20 = Fixed Exp20
type Fixed21 = Fixed Exp21
type Fixed22 = Fixed Exp22
type Fixed23 = Fixed Exp23
type Fixed24 = Fixed Exp24
type Fixed25 = Fixed Exp25
type Fixed26 = Fixed Exp26
type Fixed27 = Fixed Exp27
type Fixed28 = Fixed Exp28
type Fixed29 = Fixed Exp29

type Fixed30 = Fixed Exp30
type Fixed31 = Fixed Exp31
type Fixed32 = Fixed Exp32
type Fixed33 = Fixed Exp33
type Fixed34 = Fixed Exp34
type Fixed35 = Fixed Exp35
type Fixed36 = Fixed Exp36
type Fixed37 = Fixed Exp37
type Fixed38 = Fixed Exp38

