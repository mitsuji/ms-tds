{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

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


import Data.Monoid ((<>))
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Fixed (HasResolution(..),Fixed(..))


-- data Exp0
-- ...
-- data Exp38
#if MIN_VERSION_template_haskell(2,11,0)
returnQ $ (flip map) [0..38] $ \i -> DataD [] (mkName $ "Exp" <> (show i)) [] Nothing [] []
#else
returnQ $ (flip map) [0..38] $ \i -> DataD [] (mkName $ "Exp" <> (show i)) [] [] []
#endif


-- instance HasResolution Exp0 where resolution _  = 1
-- ...
-- instance HasResolution Exp38 where resolution _ = 100000000000000000000000000000000000000
#if MIN_VERSION_template_haskell(2,11,0)
returnQ $ (flip map) [0..38] $ \i ->
  InstanceD Nothing [] (AppT (ConT ''HasResolution) (ConT $ (mkName $ "Exp" <> (show i))))
  [FunD 'resolution [Clause [WildP] (NormalB (LitE (IntegerL (10^i)))) []]]
#else
returnQ $ (flip map) [0..38] $ \i ->
  InstanceD [] (AppT (ConT ''HasResolution) (ConT $ (mkName $ "Exp" <> (show i))))
  [FunD 'resolution [Clause [WildP] (NormalB (LitE (IntegerL (10^i)))) []]]
#endif


-- type Fixed0 = Fixed Exp0
-- ...
-- type Fixed38 = Fixed Exp38
returnQ $ (flip map) [0..38] $ \i -> TySynD (mkName $ "Fixed" <> (show i)) [] $
                                     AppT (ConT ''Fixed) (ConT $ mkName $ "Exp" <> (show i))


