----------------------------------------------------------------------------------------------------

{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main(main) where

import Test.Framework

import Text.StringEngine.Preprocessor

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

----------------------------------------------------------------------------------------------------

main :: IO ()
main = htfMain htf_thisModulesTests

prop_SimpleStr :: Bool
prop_SimpleStr = preprocessor "<Hello World>" == "Hello World"

prop_SimpleStrLiteral :: Bool
prop_SimpleStrLiteral = preprocessor "Hello World" == "\"Hello World\""

prop_Str :: Bool
prop_Str = preprocessor "<Hello> World" == "Hello\" World\""

prop_Nl :: Bool
prop_Nl = preprocessor "a\nb" == "\"a\\nb\""

prop_Esc0 :: Bool
prop_Esc0 = preprocessor "\\<" == "\"<\""

prop_Esc1 :: Bool
prop_Esc1 = preprocessor "\\>" == "\">\""

----------------------------------------------------------------------------------------------------


